package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
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
		Image[] images = new Image[7];

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 3);
		Rect roi = new Rect(new Point(300, 0), new Point(360, 360));

		Img display = new Img(binarized.getSrc(), true);
		Imgproc.rectangle(display.getSrc(), roi.br(), roi.tl(), new Scalar(255));
		images[0] = display.toJfxImage();

		Img strip = new Img(new Mat(binarized.getSrc(), roi));
		// System.out.println(strip.getSrc());
		Mat mat = Mat.zeros(360, 640, CvType.CV_8UC1);
		strip.getSrc().copyTo(new Mat(mat, roi));
		// System.out.println(mat);
		images[1] = new Img(mat, false).toJfxImage();

		Mat radon = RadonTransform.transform(strip.getSrc());
		images[2] = new Img(radon, false).toJfxImage();

		Mat projectionMap = RadonTransform.projectionMap(radon);
		// Mat projectionMapLarge = Mat.zeros(360, 640, CvType.CV_64FC1);
		// projectionMap.copyTo(new Mat(projectionMapLarge, new Rect(new Point(0, 0), new Point(projectionMap.width(), projectionMap.height()))));
		images[3] = new Img(projectionMap, false).toJfxImage();

		// Imgproc.Sobel(projectionMap, projectionMap, CvType.CV_64FC1, 0, 1);
		DirectionalFilter.cleanContour(projectionMap);
		// Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2)));
		// Imgproc.threshold(projectionMap, projectionMap, 50, 100000, Imgproc.THRESH_BINARY);
		images[4] = new Img(projectionMap, false).toJfxImage();

		int[] traj = RadonTransform.bestTraject(projectionMap, -10000);
		// System.out.println(Arrays.toString(traj));

		Mat trajs = Mat.zeros(projectionMap.size(), CvType.CV_8UC3);
		// Mat lines = Mat.zeros(superFrame.getFrame().size(), CvType.CV_8UC3);
		for (int k = 0; k < trajs.rows(); k++) {
			trajs.put(k, traj[k], 0, 0, 255);
			if (projectionMap.get(k, traj[k])[0] > 70) {
				double angle = (((double) (traj[k]) - 45) / 180 * Math.PI);
				Imgproc.line(superFrame.getFrame().getSrc(), new Point(330 - Math.cos(angle) * 30, k - Math.sin(angle) * 30), new Point(330 + Math.cos(angle) * 30, k + Math.sin(angle) * 30), new Scalar(0, 255, 0));
			}
		}

		images[5] = new Img(trajs, false).toJfxImage();
		images[6] = new Img(superFrame.getFrame().getSrc(), false).toJfxImage();

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
