package org.genericsystem.cv.application.textdetection;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.BoundedScheduledThreadPoolExecutor;
import org.genericsystem.cv.application.Config;
import org.genericsystem.cv.application.GSCapture;
import org.genericsystem.cv.application.GSVideoCapture;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class RobustTextDetectorDemo extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private final GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	private Img frame = gsCapture.read();
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
		double displaySizeReduction = 1;
		for (int col = 0; col < imageViews.length; col++)
			for (int row = 0; row < imageViews[col].length; row++) {
				ImageView imageView = new ImageView();
				imageViews[col][row] = imageView;
				mainGrid.add(imageViews[col][row], col, row);
				imageView.setFitWidth(frame.width() / displaySizeReduction);
				imageView.setFitHeight(frame.height() / displaySizeReduction);
			}
		startTimer();
	}

	Mat convertContourToMat(MatOfPoint contour) {
		Point[] pts = contour.toArray();
		Mat result = new Mat(pts.length, 2, CvType.CV_64FC1);
		for (int i = 0; i < result.rows(); ++i) {
			result.put(i, 0, pts[i].x);
			result.put(i, 1, pts[i].y);
		}
		return result;
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode)
			frame = gsCapture.read();
		Image[] images = new Image[12];
		long ref = System.currentTimeMillis();
		long ref2 = System.currentTimeMillis();
		Img gray = frame.bgr2Gray();
		RobustTextDetectorManager manager = new RobustTextDetectorManager(gray.getSrc(), 2);
		images[0] = new Img(manager.getMserMask(), false).toJfxImage();
		ref = trace("mser mask", ref);

		images[1] = new Img(manager.getCannyMask(), false).toJfxImage();
		ref = trace("canny", ref);

		images[2] = new Img(manager.getMserAndCannyMask(), false).toJfxImage();
		ref = trace("mserAndCanny", ref);

		images[3] = new Img(manager.getMserAndCannyGrownMask(), false).toJfxImage();
		ref = trace("grow", ref);

		images[4] = new Img(manager.getEdgeEnhancedMserMask(), false).toJfxImage();
		ref = trace("edgeEnhancedMser", ref);

		images[5] = new Img(manager.getEdgeEnhanceMserCCMask(), false).toJfxImage();
		ref = trace("cc", ref);

		images[6] = new Img(manager.getFilteredStrokeWidthMask(), false).toJfxImage();
		ref = trace("swt", ref);
		Mat bounding_region = new Mat();
		Imgproc.morphologyEx(manager.getFilteredStrokeWidthMask(), bounding_region, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(9, 5)));
		Imgproc.morphologyEx(bounding_region, bounding_region, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));
		// Mat result3 = new Mat();
		// superFrame.getFrame().getSrc().copyTo(result3, filtered_stroke_width);
		images[7] = new Img(bounding_region, false).toJfxImage();
		Imgproc.morphologyEx(manager.getFilteredStrokeWidthMask(), bounding_region, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(27, 5)));
		Imgproc.morphologyEx(bounding_region, bounding_region, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));
		images[8] = new Img(bounding_region, false).toJfxImage();

		ref = trace("morphologies", ref);

		Mat thresholdMask = new Mat();
		Imgproc.adaptiveThreshold(gray.getSrc(), thresholdMask, 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);
		Mat result = Mat.zeros(frame.size(), CvType.CV_8UC1);
		thresholdMask.copyTo(result, bounding_region);
		images[9] = new Img(result, false).toJfxImage();
		ref = trace("masked threshold", ref);

		ref2 = trace("total", ref2);

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
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}

}
