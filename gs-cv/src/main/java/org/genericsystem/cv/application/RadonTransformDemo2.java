package org.genericsystem.cv.application;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Range;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

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

		images[0] = binarized.toJfxImage();

		ref = trace("Binarization", ref);

		int stripWidth = 46;
		Mat vStrip = RadonTransform.extractStrip(binarized.getSrc(), binarized.width() / 2 - stripWidth / 2, stripWidth);

		Mat vStripDisplay = Mat.zeros(binarized.size(), binarized.type());
		Mat roi = new Mat(vStripDisplay, new Range(0, binarized.height()), new Range(binarized.width() / 2 - stripWidth / 2, binarized.width() / 2 + stripWidth / 2));
		vStrip.copyTo(roi);
		images[1] = new Img(vStripDisplay, false).toJfxImage();
		ref = trace("Extract strip", ref);

		Mat hough = RadonTransform.fastHoughTransform(vStrip, stripWidth);
		images[3] = new Img(hough, false).toJfxImage();
		ref = trace("FastHoughTransform", ref);
		System.out.println(hough);
		Imgproc.morphologyEx(hough, hough, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2)));
		// Core.normalize(houghTransform, houghTransform, 0, 255, Core.NORM_MINMAX);
		images[4] = new Img(hough, false).toJfxImage();

		TrajectStep[] houghVtraj = RadonTransform.bestTraject(hough, -10000, 3);
		Mat vHoughColor = Mat.zeros(hough.size(), CvType.CV_8UC3);
		hough.release();
		for (int y = 0; y < vHoughColor.height(); y++)
			vHoughColor.put(y, houghVtraj[y].theta, 0, 0, 255);
		ref = trace("Best traject hough", ref);
		Function<Double, Double> approxHoughVFunction = RadonTransform.approxTraject(houghVtraj);
		for (int y = 0; y < vHoughColor.height(); y++) {
			int x = (int) Math.round(approxHoughVFunction.apply((double) y));
			assert x >= 0 && x < vHoughColor.width() : x + " " + vHoughColor.width();
			vHoughColor.put(y, x, 0, 255, 0);
		}
		ref = trace("Display approx radon", ref);
		images[5] = new Img(vHoughColor, false).toJfxImage();

		Mat vTransform = RadonTransform.transform(vStrip, 45, 45);
		Mat vProjection = RadonTransform.projectionMap(vTransform, 45);
		images[6] = new Img(vProjection, false).toJfxImage();
		System.out.println(vProjection);
		ref = trace("Radon + Projection", ref);

		Imgproc.morphologyEx(vProjection, vProjection, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2)));
		// Core.normalize(vProjection, vProjection, 0, 255, Core.NORM_MINMAX);
		images[7] = new Img(vProjection, false).toJfxImage();

		TrajectStep[] vtraj = RadonTransform.bestTraject(vProjection, -10000, 3);
		Mat vProjectionColor = Mat.zeros(vProjection.size(), CvType.CV_8UC3);
		for (int y = 0; y < vProjectionColor.height(); y++)
			vProjectionColor.put(y, vtraj[y].theta, 0, 0, 255);
		ref = trace("Best traject radon", ref);
		Function<Double, Double> approxRadonVFunction = RadonTransform.approxTraject(vtraj);
		for (int y = 0; y < vProjectionColor.height(); y++) {
			int x = (int) Math.round(approxRadonVFunction.apply((double) y));
			assert x >= 0 && x < vProjectionColor.width() : x + " " + vProjectionColor.width();
			vProjectionColor.put(y, x, 255, 0, 0);
			x = (int) Math.round(approxHoughVFunction.apply((double) y) / (2 * stripWidth - 1) * 90);
			assert x >= 0 && x < vProjectionColor.width() : x + " " + vProjectionColor.width();
			vProjectionColor.put(y, x, 0, 255, 0);
		}
		ref = trace("Display approx radon", ref);
		images[8] = new Img(vProjectionColor, false).toJfxImage();

		// ref = trace("Compute radons", ref);
		//
		// List<Mat> vProjectionMaps = vRadons.stream().map(radon -> RadonTransform.projectionMap(radon, minAngle)).collect(Collectors.toList());
		// List<Mat> hProjectionMaps = hRadons.stream().map(radon -> RadonTransform.projectionMap(radon, minAngle)).collect(Collectors.toList());
		//
		// ref = trace("Compute projections", ref);
		//
		// vProjectionMaps.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		// hProjectionMaps.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		//
		// ref = trace("Compute gradients", ref);
		//
		// List<int[]> vTrajs = vProjectionMaps.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -10000, 3)).collect(Collectors.toList());
		// List<int[]> hTrajs = hProjectionMaps.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -10000, 3)).collect(Collectors.toList());
		// ref = trace("Compute trajects", ref);
		// List<Function<Double, Double>> approxVFunctions = vTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		// List<Function<Double, Double>> approxHFunctions = hTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		//
		// ref = trace("Compute approxs", ref);
		//
		// int hStep = stripHeight / 2;
		// int vStrip = 0;
		// List<OrientedPoint> horizontals = new ArrayList<>();
		// for (Function<Double, Double> f : approxVFunctions)
		// horizontals.addAll(RadonTransform.toHorizontalOrientedPoints(f, vStrip++, stripWidth, binarized.height(), hStep, minAngle));
		// int vStep = stripWidth / 2;
		// int hStrip = 0;
		// List<OrientedPoint> verticals = new ArrayList<>();
		// for (Function<Double, Double> f : approxHFunctions)
		// verticals.addAll(RadonTransform.toVerticalOrientedPoints(f, hStrip++, stripHeight, binarized.width(), vStep, minAngle));
		//
		// GeneralInterpolator interpolator = new GeneralInterpolator(horizontals, verticals, 6, 20);
		//
		// ref = trace("Prepare interpolator", ref);
		//
		// Img frameDisplay = superFrame.getDisplay();
		// vStrip = 0;
		// for (Function<Double, Double> f : approxVFunctions) {
		// for (int k = hStep; k + hStep <= binarized.height(); k += hStep) {
		// double angle = (f.apply((double) k) - minAngle) / 180 * Math.PI;
		// Imgproc.line(frameDisplay.getSrc(), new Point((vStrip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 6, k - Math.sin(angle) * stripWidth / 6),
		// new Point((vStrip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 6, k + Math.sin(angle) * stripWidth / 6), new Scalar(0, 255, 0), 2);
		// angle = interpolator.interpolateHorizontals((vStrip + 1) * stripWidth / 2, k);
		// Imgproc.line(frameDisplay.getSrc(), new Point((vStrip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 6, k - Math.sin(angle) * stripWidth / 6),
		// new Point((vStrip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 6, k + Math.sin(angle) * stripWidth / 6), new Scalar(255, 0, 0), 2);
		// }
		// vStrip++;
		// }
		// hStrip = 0;
		// for (Function<Double, Double> f : approxHFunctions) {
		// for (int k = vStep; k + vStep <= binarized.width(); k += vStep) {
		// double angle = (90 + minAngle - f.apply((double) k)) / 180 * Math.PI;
		// Imgproc.line(frameDisplay.getSrc(), new Point(k - Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 - Math.sin(angle) * stripHeight / 6),
		// new Point(k + Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 + Math.sin(angle) * stripHeight / 6), new Scalar(0, 0, 255), 2);
		// angle = interpolator.interpolateVerticals(k, (hStrip + 1) * stripHeight / 2);
		// Imgproc.line(frameDisplay.getSrc(), new Point(k - Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 - Math.sin(angle) * stripHeight / 6),
		// new Point(k + Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 + Math.sin(angle) * stripHeight / 6), new Scalar(255, 0, 0), 2);
		//
		// }
		// hStrip++;
		// }
		// images[1] = frameDisplay.toJfxImage();
		//
		// ref = trace("Display lines", ref);
		//
		// MeshGrid meshGrid = new MeshGrid(new Size(16, 9), interpolator, 20, 20, superFrame.getFrame().getSrc());
		// meshGrid.build();
		// ref = trace("Build mesh", ref);
		// images[3] = new Img(meshGrid.drawOnCopy(new Scalar(0, 255, 0)), false).toJfxImage();
		// ref = trace("Draw mesh", ref);
		//
		// Img dewarp = new Img(meshGrid.dewarp()).adaptativeGaussianInvThreshold(7, 3);
		// images[4] = dewarp.toJfxImage();
		// ref = trace("Dewarp", ref);
		//
		// // images[7] = new Img(RadonTransform.estimateBaselines(superFrame.getFrame().getSrc(), 0), false).toJfxImage();

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
