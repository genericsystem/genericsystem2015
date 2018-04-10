package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
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

		long ref = System.currentTimeMillis();

		Img binarized = superFrame.getFrame().gaussianBlur(new Size(3, 3)).adaptativeGaussianInvThreshold(3, 2);
		images[0] = binarized.toJfxImage();

		long last = System.currentTimeMillis();
		System.out.println("Binarization : " + (last - ref));
		ref = last;

		int stripWidth = 100;
		List<Mat> strips = RadonTransform.extractStrips(binarized.getSrc(), stripWidth);

		last = System.currentTimeMillis();
		System.out.println("Extract strips : " + (last - ref));
		ref = last;

		List<Mat> radons = strips.stream().map(strip -> RadonTransform.transform(strip, 45)).collect(Collectors.toList());

		last = System.currentTimeMillis();
		System.out.println("Compute radons : " + (last - ref));
		ref = last;

		List<Mat> projectionMaps = radons.stream().map(radon -> RadonTransform.projectionMap(radon)).collect(Collectors.toList());

		last = System.currentTimeMillis();
		System.out.println("Compute projections : " + (last - ref));
		ref = last;

		projectionMaps.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));

		last = System.currentTimeMillis();
		System.out.println("Compute gradients : " + (last - ref));
		ref = last;

		List<int[]> trajs = projectionMaps.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -5000, 2)).collect(Collectors.toList());

		List<Function<Double, Double>> approxFunctions = trajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());

		last = System.currentTimeMillis();
		System.out.println("Compute approx : " + (last - ref));
		ref = last;

		Img frameDisplay = superFrame.getDisplay();
		int strip = 0;
		for (Function<Double, Double> f : approxFunctions) {
			for (int k = 0; k < binarized.height(); k += 50) {
				double angle = (f.apply((double) k) - 45) / 180 * Math.PI;
				Imgproc.line(frameDisplay.getSrc(), new Point((strip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 2, k - Math.sin(angle) * stripWidth / 2),
						new Point((strip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 2, k + Math.sin(angle) * stripWidth / 2), new Scalar(0, 255, 0), 1);
			}
			strip++;
		}
		images[1] = frameDisplay.toJfxImage();

		last = System.currentTimeMillis();
		System.out.println("Display lines : " + (last - ref));
		ref = last;

		strip = 0;
		List<OrientedPoint> horizontals = new ArrayList<>();
		for (Function<Double, Double> f : approxFunctions) {
			for (int k = 0; k < binarized.height(); k += 50) {
				double angle = (f.apply((double) k) - 45) / 180 * Math.PI;
				horizontals.add(new OrientedPoint(new Point((strip + 1) * stripWidth / 2, k), angle, 1));
			}
			strip++;
		}

		strip = 0;
		List<OrientedPoint> verticals = new ArrayList<>();
		for (Function<Double, Double> f : approxFunctions) {
			for (int k = 0; k < binarized.height(); k += 20) {
				double angle = (f.apply((double) k) - 45) / 180 * Math.PI;
				verticals.add(new OrientedPoint(new Point((strip + 1) * stripWidth / 2, k), Math.PI / 2, 1));
			}
			strip++;
		}

		Mat frame2 = superFrame.getFrame().getSrc().clone();
		GeneralInterpolator interpolator = new GeneralInterpolator(horizontals, verticals, 2);
		last = System.currentTimeMillis();
		System.out.println("Prepare interpolator : " + (last - ref));
		ref = last;
		MeshGrid meshGrid = new MeshGrid(new Size(3, 2), interpolator, 100, 100, frame2);
		meshGrid.build();
		last = System.currentTimeMillis();
		System.out.println("Build mesh : " + (last - ref));
		ref = last;
		meshGrid.draw(frame2, new Scalar(0, 255, 0));
		last = System.currentTimeMillis();
		System.out.println("Draw mesh : " + (last - ref));
		ref = last;
		images[3] = new Img(frame2, false).toJfxImage();

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
