package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.lm.LevenbergImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class RadonTransformDemo extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private final double f = 6.053 / 0.009;

	private final GSCapture gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
	private SuperFrameImg superFrame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };

	private void startTimer() {
		timer.scheduleAtFixedRate(() -> {
			try {
				Image[] images = doWork();
				if (images != null)
					Platform.runLater(() -> {
						Iterator<Image> it = Arrays.asList(images).iterator();
						for (int row = 0; row < imageViews.length; row++)
							for (int col = 0; col < imageViews[row].length; col++)
								if (it.hasNext())
									imageViews[row][col].setImage(it.next());
					});
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}, 30, 30, TimeUnit.MILLISECONDS);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 1.5;
		for (int col = 0; col < imageViews.length; col++)
			for (int row = 0; row < imageViews[col].length; row++) {
				ImageView imageView = new ImageView();
				imageViews[col][row] = imageView;
				mainGrid.add(imageViews[col][row], col, row);
				imageView.setFitWidth(superFrame.width() / displaySizeReduction);
				imageView.setFitHeight(superFrame.height() / displaySizeReduction);
			}
		startTimer();
	}

	private Image[] doWork() {
		System.out.println("do work");
		if (!config.stabilizedMode)
			superFrame = gsCapture.read();
		Image[] images = new Image[8];

		Img binarized = superFrame.getFrame().gaussianBlur(new Size(3, 3)).adaptativeGaussianInvThreshold(3, 2);
		// Rect roi = new Rect(new Point(300, 0), new Point(360, 360));

		// Img display = new Img(binarized.getSrc(), true);
		// Imgproc.rectangle(display.getSrc(), roi.br(), roi.tl(), new Scalar(255));
		images[0] = binarized.toJfxImage();

		int stripWidth = 40;
		List<Mat> strips = RadonTransform.extractStrips(binarized.getSrc(), stripWidth);
		List<Mat> radons = strips.stream().map(strip -> RadonTransform.transform(strip, 45)).collect(Collectors.toList());
		List<Mat> projectionMaps = radons.stream().map(radon -> RadonTransform.projectionMap(radon)).collect(Collectors.toList());

		// Mat projectionMapLarge = Mat.zeros(360, 640, CvType.CV_64FC1);
		// projectionMap.copyTo(new Mat(projectionMapLarge, new Rect(new Point(0, 0), new Point(projectionMap.width(), projectionMap.height()))));
		// images[1] = new Img(projectionMap, false).toJfxImage();

		// Imgproc.Sobel(projectionMap, projectionMap, CvType.CV_64FC1, 0, 1);
		// DirectionalFilter.cleanContour(projectionMap);
		projectionMaps.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		// Imgproc.GaussianBlur(projectionMap, projectionMap, new Size(21, 21), 0);

		// Imgproc.threshold(projectionMap, projectionMap, 50, 255, Imgproc.THRESH_TOZERO);
		// images[4] = new Img(projectionMap, false).toJfxImage();

		List<int[]> trajs = projectionMaps.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -5000, 2)).collect(Collectors.toList());

		BiFunction<Double, double[], Double> f = (x, params) -> params[0] + params[1] * x + params[2] * x * x;

		List<double[]> approxParams = new ArrayList<>();
		for (int[] traj : trajs) {
			List<double[]> values = new ArrayList<>();
			for (int k = 0; k < binarized.height(); k++)
				values.add(new double[] { k, traj[k] });
			approxParams.add(LevenbergImpl.fromBiFunction(f, values, new double[] { 0, 0, 0 }).getParams());
		}
		// System.out.println(Arrays.toString(traj));

		Img frameDisplay = superFrame.getDisplay();
		int strip = 0;
		for (double[] approxParam : approxParams) {
			for (int k = 0; k < binarized.height(); k += 20) {
				double angle = (f.apply((double) k, approxParam) - 45) / 180 * Math.PI;
				Imgproc.line(frameDisplay.getSrc(), new Point((strip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 2, k - Math.sin(angle) * stripWidth / 2),
						new Point((strip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 2, k + Math.sin(angle) * stripWidth / 2), new Scalar(0, 255, 0), 1);
			}
			strip++;
		}
		images[1] = frameDisplay.toJfxImage();

		// // Mat lines = Mat.zeros(superFrame.getFrame().size(), CvType.CV_8UC3);
		// for (int k = 0; k < trajs.rows(); k++) {
		// trajs.put(k, traj[k], 0, 0, 255);
		// double approx = f.apply((double) k, params);
		// if (approx < 0)
		// approx = 0;
		// if (approx >= 2 * 45)
		// approx = 2 * 45 - 1;
		// trajs.put(k, (int) Math.round(approx), 0, 255, 0);
		// if (k % 10 == 0) {
		// double angle = (approx - 45) / 180 * Math.PI;
		// Imgproc.line(superFrame.getFrame().getSrc(), new Point(330 - Math.cos(angle) * 30, k - Math.sin(angle) * 30), new Point(330 + Math.cos(angle) * 30, k + Math.sin(angle) * 30), new Scalar(0, 255, 0), 2);
		// }
		// }
		//
		// images[5] = new Img(trajs, false).toJfxImage();
		// images[6] = new Img(superFrame.getFrame().getSrc(), false).toJfxImage();

		// images[7] = new Img(RadonTransform.estimateBaselines(superFrame.getFrame().getSrc(), 0), false).toJfxImage();

		return images;
	}

	@Override
	protected void onS() {
		config.stabilizedMode = !config.stabilizedMode;
	}

	@Override
	protected void onSpace() {
		if (config.isOn)
			timer.shutdown();
		else {
			timer = new BoundedScheduledThreadPoolExecutor();
			startTimer();
		}
		config.isOn = !config.isOn;
	}

	@Override
	protected void onT() {
		config.textsEnabledMode = !config.textsEnabledMode;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}

}
