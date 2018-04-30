package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Range;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class RadonTransformDemo2 extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private final double f = 6.053 / 0.009;

	private GSCapture gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
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
		}, 1000, 30, TimeUnit.MILLISECONDS);
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
		if (!config.stabilizedMode) {
			superFrame = gsCapture.read();
		}
		Image[] images = new Image[9];

		long ref = System.currentTimeMillis();

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 5);
		// Img binarized = new Img(Mat.zeros(360, 640, CvType.CV_8UC1), false);
		double[] angles = { -10, -25, -5 };
		int count = 0;
		for (int y = 100; y <= 260; y += 80) {
			double angle = angles[count] / 180 * Math.PI;
			// Imgproc.line(binarized.getSrc(), new Point(320 - 40 * Math.cos(angle), y - 40 * Math.sin(angle)), new Point(320 + 40 * Math.cos(angle), y + 40 * Math.sin(angle)), new Scalar(255));
			count++;
		}

		images[0] = binarized.toJfxImage();

		ref = trace("Binarization", ref);

		int stripWidth = 72;
		Mat vStrip = RadonTransform.extractStrip(binarized.getSrc(), binarized.width() / 2 - stripWidth / 2, stripWidth);

		Mat vStripDisplay = Mat.zeros(binarized.size(), binarized.type());
		Mat roi = new Mat(vStripDisplay, new Range(0, binarized.height()), new Range(binarized.width() / 2 - stripWidth / 2, binarized.width() / 2 + stripWidth / 2));
		vStrip.copyTo(roi);
		images[1] = new Img(vStripDisplay, false).toJfxImage();
		ref = trace("Extract strip", ref);

		Mat houghTransform = RadonTransform.fastHoughTransform(vStrip);
		ref = trace("FHT", ref);
		// Imgproc.resize(houghTransform, houghTransform, new Size(91, houghTransform.height()), 0, 0, Imgproc.INTER_LINEAR);
		// Mat hough = RadonTransform.fhtRemap(houghTransform, stripWidth);
		images[3] = new Img(houghTransform, false).toJfxImage();

		Imgproc.morphologyEx(houghTransform, houghTransform, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2)));
		Core.normalize(houghTransform, houghTransform, 0, 255, Core.NORM_MINMAX);
		images[4] = new Img(houghTransform, false).toJfxImage();

		ref = trace("FHT compute", ref);
		TrajectStep[] houghVtraj = RadonTransform.bestTraject(houghTransform, -1000, 2);

		for (int y = 0; y < houghVtraj.length; y++)
			houghVtraj[y].theta = (int) Math.round(Math.atan((double) (houghVtraj[y].theta - stripWidth + 1) / (stripWidth - 1)) / Math.PI * 180 + 45);

		Mat vHoughColor = Mat.zeros(houghTransform.height(), 91, CvType.CV_8UC3);
		for (int y = 0; y < vHoughColor.height(); y++) {
			vHoughColor.put(y, houghVtraj[y].theta, 0, 0, 255);
			if (houghVtraj[y].magnitude >= 20) {
				vHoughColor.put(y, houghVtraj[y].theta, 255, 0, 0);
			}
		}
		ref = trace("Best traject hough", ref);

		Function<Double, Double> approxHoughVFunction = RadonTransform.approxTraject(houghVtraj);
		for (int y = 0; y < vHoughColor.height(); y++) {
			int x = (int) Math.round(approxHoughVFunction.apply((double) y));
			if (x < 0)
				x = 0;
			if (x >= vHoughColor.width())
				x = vHoughColor.width() - 1;
			vHoughColor.put(y, x, 0, 255, 0);
		}
		System.out.println(houghTransform);

		// for (Pair pair : RadonTransform.getLocalExtr(houghTransform, 200)) {
		// Imgproc.circle(vHoughColor, new Point((Math.atan((pair.point.x - stripWidth + 1) / (stripWidth - 1)) / Math.PI * 180) + 45, pair.point.y), 2, new Scalar(255, 0, 0), -1);
		// System.out.println((Math.atan((pair.point.x - stripWidth + 1) / (stripWidth - 1)) / Math.PI * 180) + " " + pair.point.y + " " + pair.value);
		// }

		houghTransform.release();

		ref = trace("Display approx hough", ref);
		images[5] = new Img(vHoughColor, false).toJfxImage();

		Mat vTransform = RadonTransform.radonTransform(vStrip, -45, 45);
		Mat vProjection = RadonTransform.radonRemap(vTransform, -45);

		// for (Pair pair : RadonTransform.getLocalExtr(vProjection, 100)) {
		// Imgproc.circle(vHoughColor, pair.point, 2, new Scalar(255, 0, 0), -1);
		// System.out.println((Math.atan((pair.point.x - 45) / (45)) / Math.PI * 180) + " " + pair.point.y + " " + pair.value);
		// }

		images[6] = new Img(vProjection, false).toJfxImage();
		System.out.println(vProjection);
		Imgproc.morphologyEx(vProjection, vProjection, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2)));
		Core.normalize(vProjection, vProjection, 0, 255, Core.NORM_MINMAX);
		ref = trace("Radon + Projection", ref);
		images[7] = new Img(vProjection, false).toJfxImage();

		TrajectStep[] vtraj = RadonTransform.bestTraject(vProjection, -2000, 2);
		Mat vProjectionColor = Mat.zeros(vProjection.size(), CvType.CV_8UC3);
		for (int y = 0; y < vProjectionColor.height(); y++)
			vProjectionColor.put(y, vtraj[y].theta, 0, 0, 255);
		ref = trace("Best traject radon", ref);
		Function<Double, Double> approxRadonVFunction = RadonTransform.approxTraject(vtraj);
		for (int y = 0; y < vProjectionColor.height(); y++) {
			int x = (int) Math.round(approxRadonVFunction.apply((double) y));
			if (x < 0)
				x = 0;
			if (x >= vProjectionColor.width())
				x = vProjectionColor.width() - 1;
			vProjectionColor.put(y, x, 255, 0, 0);
			x = (int) Math.round(approxHoughVFunction.apply((double) y));
			if (x < 0)
				x = 0;
			if (x >= vProjectionColor.width())
				x = vProjectionColor.width() - 1;
			vProjectionColor.put(y, x, 0, 255, 0);
		}

		double houghError = 0;
		double houghApproxError = 0;
		double radonError = 0;
		double radonApproxError = 0;
		count = 0;
		for (int y = 100; y <= 260; y += 80) {
			houghError += Math.pow((houghVtraj[100].theta - 45) - angles[count], 2);
			houghApproxError += Math.pow(approxHoughVFunction.apply(100d) - 45 - angles[count], 2);
			radonError += Math.pow((vtraj[100].theta - 45) - angles[count], 2);
			radonApproxError += Math.pow(approxRadonVFunction.apply(100d) - 45 - angles[count], 2);
			count++;
		}
		houghError = Math.sqrt(houghError);
		houghApproxError = Math.sqrt(houghApproxError);
		radonError = Math.sqrt(radonError);
		radonApproxError = Math.sqrt(radonApproxError);

		System.out.println("Hough        : " + houghError);
		System.out.println("Hough approx : " + houghApproxError);

		System.out.println("Radon        : " + radonError);
		System.out.println("Radon approx : " + radonApproxError);

		ref = trace("Display approx radon", ref);
		images[8] = new Img(vProjectionColor, false).toJfxImage();

		return images;

	}

	private long trace(String message, long ref) {
		long last = System.currentTimeMillis();
		System.out.println(message + " : " + (last - ref));
		return last;
	}

	@Override
	protected void onS() {
		config.stabilizedMode = !config.stabilizedMode;
	}

	@Override
	protected void onSpace() {
		if (config.isOn) {
			timer.shutdown();
			// gsCapture.release();
		} else {
			timer = new BoundedScheduledThreadPoolExecutor();
			// gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
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
