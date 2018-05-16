package org.genericsystem.cv.application;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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
		double displaySizeReduction = 1;
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
		Mat vStrip2 = RadonTransform.extractStrip(binarized.getSrc(), binarized.width() / 2 - stripWidth / 2 + w, stripWidth);
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
		Core.normalize(houghTransform, houghTransform, 0, 255, Core.NORM_MINMAX);
		// Core.normalize(houghTransform2, houghTransform2, 0, 255, Core.NORM_MINMAX);

		Mat blur = new Mat();
		Imgproc.blur(houghTransform, blur, new Size(1, 11), new Point(-1, -1), Core.BORDER_ISOLATED);
		images[2] = new Img(blur, false).toJfxImage();
		blur.release();

		images[3] = new Img(houghTransform, false).toJfxImage();
		ref = trace("FHT", ref);

		Mat adaptive = RadonTransform.adaptivHough(houghTransform, 11);
		images[4] = new Img(adaptive, false).toJfxImage();
		adaptive.release();
		ref = trace("Adaptive FHT", ref);

		List<HoughTrajectStep> magnitudes = RadonTransform.bestTrajectFHT(houghTransform, 11, -1);
		// for (int y = 0; y < magnitudes.size(); y++) {
		// if (magnitudes.get(y).magnitude <= 1)
		// for (int end = y + 1; end < magnitudes.size(); end++) {
		// if (magnitudes.get(end).magnitude > 1) {
		// for (int current = y; current < end; current++)
		// magnitudes.get(current).theta = magnitudes.get(y == 0 ? 0 : y - 1).theta + (magnitudes.get(end).theta - magnitudes.get(y == 0 ? 0 : y - 1).theta) * (current - y) / (end - y + 1);
		// y = end;
		// break;
		// }
		// }
		// }

		Mat trajectDisplay = Mat.zeros(houghTransform.height(), 90, CvType.CV_8UC3);
		for (HoughTrajectStep step : magnitudes)
			if (step.magnitude >= 1)
				trajectDisplay.put(step.y, (int) Math.round(step.getTheta()), 255, 0, 0);
			else
				trajectDisplay.put(step.y, (int) Math.round(step.getTheta()), 0, 0, 255);
		images[5] = new Img(trajectDisplay, false).toJfxImage();
		ref = trace("FHT traject", ref);

		Mat magnitudesDisplay = Mat.zeros(magnitudes.size(), 255, CvType.CV_8UC1);
		for (HoughTrajectStep step : magnitudes)
			Imgproc.line(magnitudesDisplay, new Point(0, step.y), new Point(step.magnitude, step.y), new Scalar(255));
		images[6] = new Img(magnitudesDisplay, false).toJfxImage();
		ref = trace("Display magnitudes", ref);

		List<HoughTrajectStep> sortedMagnitudes = new ArrayList<>(magnitudes);
		Collections.sort(sortedMagnitudes);

		double alpha = 0.1;
		double t = 0.5;
		Set<HoughTrajectStep> alreadyComputed = new HashSet<>();
		double max = magnitudes.stream().mapToDouble(ts -> ts.magnitude).max().getAsDouble();
		System.out.println("Max : " + max);

		List<int[]> result = new ArrayList<>();
		for (HoughTrajectStep trajectStep : magnitudes.stream().sorted().collect(Collectors.toList())) {
			if (trajectStep.magnitude < alpha * max)
				break;
			if (!alreadyComputed.contains(trajectStep)) {
				double tAlpha = t * trajectStep.magnitude;
				assert trajectStep.y < magnitudes.size();
				int y1 = trajectStep.y;
				for (; y1 >= 0; y1--)
					if (magnitudes.get(y1).magnitude < tAlpha)
						break;
				y1++;
				assert y1 >= 0;
				int y2 = trajectStep.y;
				for (; y2 < magnitudes.size(); y2++)
					if (magnitudes.get(y2).magnitude < tAlpha)
						break;
				y2--;
				assert y2 < magnitudes.size();
				int[] r = new int[] { y1, y2 };

				boolean alreadyVisited = false;
				for (int k = y1; k <= y2; k++) {
					if (alreadyComputed.contains(magnitudes.get(k))) {
						alreadyVisited = true;
						break;
					}
				}
				if (!alreadyVisited)
					result.add(r);
				alreadyComputed.add(magnitudes.get(y1));
				alreadyComputed.add(magnitudes.get(y2));
			}
		}
		Mat rangeDisplay = magnitudesDisplay.clone();
		for (int[] y1y2 : result) {
			double minMagnitude = Double.MAX_VALUE;
			for (int k = y1y2[0]; k <= y1y2[1]; k++)
				if (minMagnitude > magnitudes.get(k).magnitude)
					minMagnitude = magnitudes.get(k).magnitude;
			// System.out.println("minMagnitude : " + minMagnitude + " Range : " + y1y2[0] + " " + y1y2[1]);
			Imgproc.line(rangeDisplay, new Point(minMagnitude, y1y2[0]), new Point(minMagnitude, y1y2[1]), new Scalar(0), 3);
		}

		// for (int row = 0; row < rangeDisplay.rows(); row++)
		// Imgproc.line(rangeDisplay, new Point(0, row), new Point(magnitudes.get(row).magnitude, row), new Scalar(255));
		images[7] = new Img(rangeDisplay, false).toJfxImage();
		ref = trace("Display ranged magnitudes", ref);

		Mat vStripColor = new Mat();
		Imgproc.cvtColor(vStrip, vStripColor, Imgproc.COLOR_GRAY2BGR);
		for (int[] y1y2 : result) {
			// for (TrajectStep trajectStep : Arrays.stream(houghVtraj).filter(ts -> Math.abs(magnitudes.get(ts.k).magnitude) > 30).collect(Collectors.toList())) {
			double mag = stripWidth;
			int row = y1y2[0];
			double theta = (magnitudes.get(row).getTheta() - 45) / 180 * Math.PI;
			Scalar color = new Scalar(0, 255, 0);
			Imgproc.line(vStripColor, new Point(vStripColor.width() / 2 - mag * Math.cos(theta), row - mag * Math.sin(theta)), new Point(vStripColor.width() / 2 + mag * Math.cos(theta), row + mag * Math.sin(theta)), color, 1);
			color = new Scalar(0, 0, 255);
			row = y1y2[1];
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
