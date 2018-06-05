package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator;
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.core.Scalar;
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
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };

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
		Image[] images = new Image[20];

		long ref = System.currentTimeMillis();

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 5);
		// Img binarized = new Img(Mat.zeros(360, 640, CvType.CV_8UC1), false);
		double[] angles = { -10, -25, -5 };
		int count = 0;
		for (int y = 100; y <= 260; y += 80) {
			double angle = angles[count] / 180 * Math.PI;
			// Imgproc.line(binarized.getSrc(), new Point(320 - 40 * Math.cos(angle), y - 40 * Math.sin(angle)), new Point(320 + 40 * Math.cos(angle), y + 40 * Math.sin(angle)), new Scalar(255), 1);
			// Imgproc.putText(binarized.getSrc(), "Hello boy", new Point(320 - 40 * Math.cos(angle), y - 40 * Math.sin(angle)), Core.FONT_HERSHEY_PLAIN, 2, new Scalar(255), 1);
			count++;
		}

		images[0] = binarized.toJfxImage();
		ref = trace("Binarization", ref);

		int stripWidth = 100;
		int w = 50;
		Mat vStrip = RadonTransform.extractStrip(binarized.getSrc(), binarized.width() / 2 - stripWidth / 2, stripWidth);
		// Mat vStrip2 = RadonTransform.extractStrip(binarized.getSrc(), binarized.width() / 2 - stripWidth / 2 + w, stripWidth);
		Mat vStripDisplay = Mat.zeros(binarized.size(), binarized.type());
		Mat roi = new Mat(vStripDisplay, new Range(0, binarized.height()), new Range(binarized.width() / 2 - stripWidth / 2, binarized.width() / 2 + stripWidth / 2));
		vStrip.copyTo(roi);
		// Mat roi2 = new Mat(vStripDisplay, new Range(0, binarized.height()), new Range(binarized.width() / 2 - stripWidth / 2 + w, binarized.width() / 2 + stripWidth / 2 + w));
		// vStrip2.copyTo(roi2);
		images[1] = new Img(vStripDisplay, false).toJfxImage();
		ref = trace("Extract strip", ref);

		Mat houghTransform = RadonTransform.fastHoughTransform(vStrip);
		// Mat houghTransform2 = RadonTransform.fastHoughTransform(vStrip2);

		System.out.println(houghTransform);
		// houghTransform.row(0).setTo(new Scalar(0));
		// houghTransform.row(houghTransform.rows() - 1).setTo(new Scalar(0));
		Core.normalize(houghTransform, houghTransform, 0, 1, Core.NORM_MINMAX);
		Mat houghTransform255 = new Mat();
		Core.normalize(houghTransform, houghTransform255, 0, 255, Core.NORM_MINMAX);

		Mat blur = new Mat();
		Imgproc.blur(houghTransform255, blur, new Size(1, 11), new Point(-1, -1), Core.BORDER_ISOLATED);
		images[2] = new Img(blur, false).toJfxImage();
		blur.release();

		images[3] = new Img(houghTransform255, false).toJfxImage();
		ref = trace("FHT", ref);

		Mat adaptive = RadonTransform.adaptivHough(houghTransform255, 11);
		images[4] = new Img(adaptive, false).toJfxImage();
		adaptive.release();
		ref = trace("Adaptive FHT", ref);

		List<HoughTrajectStep> magnitudes = RadonTransform.bestTrajectFHT(houghTransform, 11, -0.2);
		List<HoughTrajectStep> filteredMagnitudes = new ArrayList<>();
		filteredMagnitudes.add(magnitudes.get(0));
		for (int row = 1; row < magnitudes.size() - 1; row++) {
			HoughTrajectStep step = magnitudes.get(row);
			if (step.magnitude >= 0.2)
				filteredMagnitudes.add(step);
		}
		filteredMagnitudes.add(magnitudes.get(magnitudes.size() - 1));
		/// magnitudes = magnitudes.stream().filter(step -> step.magnitude >= 0.2).collect(Collectors.toList());
		PolynomialSplineFunction polynomialSplineFunction = new LinearInterpolator().interpolate(filteredMagnitudes.stream().mapToDouble(step -> step.y).toArray(), filteredMagnitudes.stream().mapToDouble(step -> step.derivative).toArray());

		// for (int y = 0; y < magnitudes.size(); y++) {
		// if (magnitudes.get(y).magnitude <= 0.2)
		// for (int end = y + 1; end < magnitudes.size(); end++) {
		// if (magnitudes.get(end).magnitude > 0.2) {
		// for (int current = y; current < end; current++)
		// magnitudes.get(current).derivative = magnitudes.get(y == 0 ? 0 : y - 1).derivative + (magnitudes.get(end).derivative - magnitudes.get(y == 0 ? 0 : y - 1).derivative) * (current - y) / (end - y + 1);
		// y = end;
		// break;
		// }
		// }
		// }
		ref = trace("FHT traject 1", ref);
		List<HoughTrajectStep> magnitudes2 = RadonTransform.bestTrajectFHT2(houghTransform, 11, 500, 30);
		ref = trace("FHT traject2", ref);
		PolynomialSplineFunction polynomialSplineFunction2 = new LinearInterpolator().interpolate(magnitudes2.stream().mapToDouble(step -> step.y).toArray(), magnitudes2.stream().mapToDouble(step -> step.derivative).toArray());

		// double ystep = ((double) houghTransform.rows() / 30);
		Mat trajectDisplay = Mat.zeros(houghTransform.height(), 200, CvType.CV_8UC3);
		for (double y = 0; y < trajectDisplay.height(); y++)
			trajectDisplay.put((int) y, (int) (100 * polynomialSplineFunction.value(y) + 100), 255, 0, 0);
		for (double y = 0; y < trajectDisplay.height(); y++)
			trajectDisplay.put((int) y, (int) (100 * polynomialSplineFunction2.value(y) + 100), 0, 0, 255);
		// for (HoughTrajectStep step : magnitudes)
		// // if (step.magnitude >= 0.2)
		// trajectDisplay.put(step.y, (int) Math.round(100d * step.derivative + 100), 255, 0, 0);
		// else
		// trajectDisplay.put(step.y, (int) Math.round(step.getTheta()), 0, 255, 0);
		images[5] = new Img(trajectDisplay, false).toJfxImage();
		ref = trace("FHT traject", ref);

		Mat magnitudesDisplay = Mat.zeros(magnitudes.size(), 255, CvType.CV_8UC1);
		for (HoughTrajectStep step : magnitudes)
			Imgproc.line(magnitudesDisplay, new Point(0, step.y), new Point(step.magnitude * 255, step.y), new Scalar(255));
		images[6] = new Img(magnitudesDisplay, false).toJfxImage();
		ref = trace("Display magnitudes", ref);

		List<HoughTrajectStep[]> lines = RadonTransform.getStripLinesFHT(magnitudes, 0.3, 0.2);
		Mat rangeDisplay = magnitudesDisplay.clone();
		for (HoughTrajectStep[] topBottom : lines) {
			double minMagnitude = Double.MAX_VALUE;
			for (int y = topBottom[0].y; y <= topBottom[1].y; y++)
				if (minMagnitude > magnitudes.get(y).magnitude)
					minMagnitude = magnitudes.get(y).magnitude;
			// System.out.println("minMagnitude : " + minMagnitude + " Range : " + y1y2[0] + " " + y1y2[1]);
			Imgproc.line(rangeDisplay, new Point(minMagnitude * 255, topBottom[0].y), new Point(minMagnitude * 255, topBottom[1].y), new Scalar(0), 1);
		}

		images[7] = new Img(rangeDisplay, false).toJfxImage();
		ref = trace("Display ranged magnitudes", ref);

		Mat vStripColor = new Mat();
		Imgproc.cvtColor(vStrip, vStripColor, Imgproc.COLOR_GRAY2BGR);
		for (HoughTrajectStep[] topBottom : lines) {
			// for (TrajectStep trajectStep : Arrays.stream(houghVtraj).filter(ts -> Math.abs(magnitudes.get(ts.k).magnitude) > 30).collect(Collectors.toList())) {
			double mag = stripWidth;
			int row = topBottom[0].y;
			double theta = (magnitudes.get(row).getTheta() - 45) / 180 * Math.PI;
			Scalar color = new Scalar(0, 255, 0);
			Imgproc.line(vStripColor, new Point(vStripColor.width() / 2 - mag * Math.cos(theta), row - mag * Math.sin(theta)), new Point(vStripColor.width() / 2 + mag * Math.cos(theta), row + mag * Math.sin(theta)), color, 1);
			color = new Scalar(0, 0, 255);
			row = topBottom[1].y;
			theta = (magnitudes.get(row).getTheta() - 45) / 180 * Math.PI;
			Imgproc.line(vStripColor, new Point(vStripColor.width() / 2 - mag * Math.cos(theta), row - mag * Math.sin(theta)), new Point(vStripColor.width() / 2 + mag * Math.cos(theta), row + mag * Math.sin(theta)), color, 1);

		}

		Mat vStripColorDisplay = Mat.zeros(binarized.size(), vStripColor.type());
		Mat stripRoi = new Mat(vStripColorDisplay, new Range(0, vStripColorDisplay.height()), new Range(vStripColorDisplay.width() / 2 - stripWidth / 2, vStripColorDisplay.width() / 2 + stripWidth / 2));
		vStripColor.copyTo(stripRoi);
		images[8] = new Img(vStripColorDisplay, false).toJfxImage();
		ref = trace("Display underlined strip", ref);

		Mat vTransform = RadonTransform.radonTransform(vStrip, -45, 45);
		Mat vProjection = RadonTransform.radonRemap(vTransform, -45);

		images[9] = new Img(vProjection, false).toJfxImage();
		System.out.println(vProjection);

		Imgproc.morphologyEx(vProjection, vProjection, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2)));
		Core.normalize(vProjection, vProjection, 0, 255, Core.NORM_MINMAX);
		ref = trace("Radon + Projection", ref);
		images[10] = new Img(vProjection, false).toJfxImage();

		TrajectStep[] vtraj = RadonTransform.bestTrajectRadon(vProjection, -20);

		for (int y = 0; y < vtraj.length; y++) {
			if (vtraj[y].magnitude == 0)
				for (int end = y + 1; end < vtraj.length; end++) {
					if (vtraj[end].magnitude != 0) {
						for (int current = y; current < end; current++)
							vtraj[current].theta = vtraj[y == 0 ? 0 : y - 1].theta + (vtraj[end].theta - vtraj[y == 0 ? 0 : y - 1].theta) * (current - y) / (end - y + 1);
						y = end;
						break;
					}
				}
		}

		Mat vProjectionColor = Mat.zeros(vProjection.size(), CvType.CV_8UC3);
		for (int y = 0; y < vProjectionColor.height(); y++)
			if (vtraj[y].magnitude != 0)
				vProjectionColor.put(y, vtraj[y].theta, 255, 0, 0);
			else
				vProjectionColor.put(y, vtraj[y].theta, 0, 0, 255);

		ref = trace("Best traject radon", ref);
		images[11] = new Img(vProjectionColor, false).toJfxImage();
		// Function<Double, Double> approxRadonVFunction = RadonTransform.approxTraject(vtraj);
		// for (int y = 0; y < vProjectionColor.height(); y++) {
		// int x = (int) Math.round(approxRadonVFunction.apply((double) y));
		// if (x < 0)
		// x = 0;
		// if (x >= vProjectionColor.width())
		// x = vProjectionColor.width() - 1;
		// vProjectionColor.put(y, x, 255, 0, 0);
		// x = (int) Math.round(approxHoughVFunction.apply((double) y));
		// if (x < 0)
		// x = 0;
		// if (x >= vProjectionColor.width())
		// x = vProjectionColor.width() - 1;
		// vProjectionColor.put(y, x, 0, 255, 0);
		// }
		// ref = trace("Approx traject radon", ref);

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
