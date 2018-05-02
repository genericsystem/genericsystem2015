package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
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

	private GSCapture gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
	private SuperFrameImg superFrame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };
	private int frameCount = 0;

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
		}, 3000, 30, TimeUnit.MILLISECONDS);
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
			frameCount++;
		}
		Image[] images = new Image[20];

		long ref = System.currentTimeMillis();

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 5);

		// Mat frameClone = superFrame.getFrame().getSrc().clone();
		// DirectionalEnhancer.drawFilteredLines(binarized.getSrc(), DirectionalEnhancer.getLines(frameClone));

		images[0] = binarized.toJfxImage();

		if (frameCount < 30)
			return images;
		Img transposedBinarized = binarized.transpose();

		ref = trace("Binarization", ref);

		int stripWidth = 64;
		List<Mat> vStrips = RadonTransform.extractStrips(binarized.getSrc(), stripWidth);
		int stripHeight = 64;
		List<Mat> hStrips = RadonTransform.extractStrips(transposedBinarized.getSrc(), stripHeight);

		ref = trace("Extract strips", ref);

		int minAngle = -45;
		int maxAngle = 45;
		List<Mat> vRadons = vStrips.stream().map(strip -> RadonTransform.radonTransform(strip, minAngle, maxAngle)).collect(Collectors.toList());
		List<Mat> hRadons = hStrips.stream().map(strip -> RadonTransform.radonTransform(strip, minAngle, maxAngle)).collect(Collectors.toList());

		ref = trace("Compute radons", ref);

		List<Mat> vProjectionMaps = vRadons.stream().map(radon -> RadonTransform.radonRemap(radon, minAngle)).collect(Collectors.toList());
		List<Mat> hProjectionMaps = hRadons.stream().map(radon -> RadonTransform.radonRemap(radon, minAngle)).collect(Collectors.toList());

		ref = trace("Compute radon remap", ref);

		vProjectionMaps.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		hProjectionMaps.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));

		List<Mat> vHoughs = vStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());

		vHoughs.stream().forEach(projectionMap -> Imgproc.resize(projectionMap, projectionMap, new Size(91, projectionMap.height()), 0, 0, Imgproc.INTER_LINEAR));
		hHoughs.stream().forEach(projectionMap -> Imgproc.resize(projectionMap, projectionMap, new Size(91, projectionMap.height()), 0, 0, Imgproc.INTER_LINEAR));

		vHoughs.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		hHoughs.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));

		ref = trace("Compute FHT", ref);

		// vHoughs.stream().map(radon -> RadonTransform.fhtRemap(radon)).collect(Collectors.toList());
		// hHoughs.stream().map(radon -> RadonTransform.fhtRemap(radon)).collect(Collectors.toList());

		ref = trace("Compute FHT remap", ref);

		List<TrajectStep[]> vTrajs = vProjectionMaps.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -1000)).collect(Collectors.toList());
		List<TrajectStep[]> hTrajs = hProjectionMaps.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -1000)).collect(Collectors.toList());
		List<TrajectStep[]> vHoughTrajs = vHoughs.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -1000)).collect(Collectors.toList());
		List<TrajectStep[]> hHoughTrajs = hHoughs.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -1000)).collect(Collectors.toList());
		ref = trace("Compute trajects", ref);
		for (TrajectStep[] houghVtraj : vHoughTrajs)
			for (int y = 0; y < houghVtraj.length; y++)
				houghVtraj[y].theta = (int) Math.round(Math.atan((double) (houghVtraj[y].theta - 45) / 45) / Math.PI * 180 + 45);

		for (TrajectStep[] houghHtraj : hHoughTrajs)
			for (int y = 0; y < houghHtraj.length; y++)
				houghHtraj[y].theta = (int) Math.round(Math.atan((double) (houghHtraj[y].theta - 45) / 45) / Math.PI * 180 + 45);

		ref = trace("Transform trajects", ref);
		List<Function<Double, Double>> approxVFunctions = vTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		List<Function<Double, Double>> approxHFunctions = hTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		List<Function<Double, Double>> approxVFHTFunctions = vHoughTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		List<Function<Double, Double>> approxHFHTFunctions = hHoughTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());

		ref = trace("Compute approxs", ref);

		List<PolynomialSplineFunction> vRadonSplinesFunctions = RadonTransform.toPolynomialSplineFunction(approxVFunctions, binarized.getSrc().size(), 20, minAngle, vStrips.size(), 0.5f);
		List<PolynomialSplineFunction> hRadonSplinesFunctions = RadonTransform.toPolynomialSplineFunction(approxHFunctions, transposedBinarized.getSrc().size(), 20, minAngle, hStrips.size(), 0.5f);
		List<PolynomialSplineFunction> vFHTSplinesFunctions = RadonTransform.toPolynomialSplineFunction(approxVFHTFunctions, binarized.getSrc().size(), 20, minAngle, vStrips.size(), 0.5f);
		List<PolynomialSplineFunction> hFHTSplinesFunctions = RadonTransform.toPolynomialSplineFunction(approxHFHTFunctions, transposedBinarized.getSrc().size(), 20, minAngle, hStrips.size(), 0.5f);

		Mat image = superFrame.getFrame().getSrc().clone();
		RadonTransform.displayHSplines(vRadonSplinesFunctions, image);
		RadonTransform.displayVSplines(hRadonSplinesFunctions, image);

		// Point[][] intersections = new SplineMeshGrid2(hRadonSplinesFunctions, vRadonSplinesFunctions,20, 20, image).build();
		// for (int col = 0; col < intersections.length; col++)
		// for (int row = 0; row < intersections[col].length; row++)
		// if (intersections[col][row] != null)
		// Imgproc.circle(image, intersections[col][row], 3, new Scalar(0, 0, 255), -1);

		Point[][] intersections = new SplineMeshGrid(20, 20, image).build(hRadonSplinesFunctions, vRadonSplinesFunctions);
		for (int col = 0; col < intersections.length; col++)
			for (int row = 0; row < intersections[col].length; row++)
				if (intersections[col][row] != null)
					Imgproc.circle(image, intersections[col][row], 3, new Scalar(0, 0, 255), -1);

		images[10] = new Img(image, false).toJfxImage();

		Mat image2 = superFrame.getFrame().getSrc().clone();
		RadonTransform.displayHSplines(vFHTSplinesFunctions, image2);
		RadonTransform.displayVSplines(hFHTSplinesFunctions, image2);
		images[11] = new Img(image2, false).toJfxImage();

		ref = trace("Compute cubic spline lines", ref);

		int hStep = stripHeight / 2;
		int vStrip = 0;
		List<OrientedPoint> horizontals = new ArrayList<>();
		for (Function<Double, Double> f : approxVFunctions)
			horizontals.addAll(RadonTransform.toHorizontalOrientedPoints(f, (vStrip++ + 1) * hStep, binarized.height(), hStep));
		int vStep = stripWidth / 2;
		int hStrip = 0;
		List<OrientedPoint> verticals = new ArrayList<>();
		for (Function<Double, Double> f : approxHFunctions)
			verticals.addAll(RadonTransform.toVerticalOrientedPoints(f, (hStrip++ + 1) * vStep, binarized.width(), vStep));

		GeneralInterpolator interpolator = new GeneralInterpolator(horizontals, verticals, 6, 20);

		vStrip = 0;
		List<OrientedPoint> fhtHorizontals = new ArrayList<>();
		for (Function<Double, Double> f : approxVFHTFunctions)
			fhtHorizontals.addAll(RadonTransform.toHorizontalOrientedPoints(f, (vStrip++ + 1) * hStep, binarized.height(), hStep));
		hStrip = 0;
		List<OrientedPoint> fhtVerticals = new ArrayList<>();
		for (Function<Double, Double> f : approxHFHTFunctions)
			fhtVerticals.addAll(RadonTransform.toVerticalOrientedPoints(f, (hStrip++ + 1) * vStep, binarized.width(), vStep));

		GeneralInterpolator interpolatorFHT = new GeneralInterpolator(fhtHorizontals, fhtVerticals, 6, 20);

		ref = trace("Prepare interpolator", ref);

		Img frameDisplay = new Img(superFrame.getFrame().getSrc().clone(), false);
		vStrip = 0;
		for (Function<Double, Double> f : approxVFunctions) {
			for (int y = hStep; y <= binarized.height(); y += hStep) {
				double angle = (f.apply((double) y) - minAngle) / 180 * Math.PI;
				Imgproc.line(frameDisplay.getSrc(), new Point((vStrip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 6, y - Math.sin(angle) * stripWidth / 6),
						new Point((vStrip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 6, y + Math.sin(angle) * stripWidth / 6), new Scalar(0, 255, 0), 2);
				angle = interpolator.interpolateVerticals((vStrip + 1) * stripWidth / 2, y);
				Imgproc.line(frameDisplay.getSrc(), new Point((vStrip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 6, y - Math.sin(angle) * stripWidth / 6),
						new Point((vStrip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 6, y + Math.sin(angle) * stripWidth / 6), new Scalar(255, 0, 0), 2);
			}
			vStrip++;
		}

		hStrip = 0;
		for (Function<Double, Double> f : approxHFunctions) {
			for (int x = vStep; x <= binarized.width(); x += vStep) {
				double angle = (90 + minAngle - f.apply((double) x)) / 180 * Math.PI;
				Imgproc.line(frameDisplay.getSrc(), new Point(x - Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 - Math.sin(angle) * stripHeight / 6),
						new Point(x + Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 + Math.sin(angle) * stripHeight / 6), new Scalar(0, 0, 255), 2);
				angle = interpolator.interpolateHorizontals(x, (hStrip + 1) * stripHeight / 2);
				Imgproc.line(frameDisplay.getSrc(), new Point(x - Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 - Math.sin(angle) * stripHeight / 6),
						new Point(x + Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 + Math.sin(angle) * stripHeight / 6), new Scalar(255, 0, 0), 2);

			}
			hStrip++;
		}

		images[1] = frameDisplay.toJfxImage();

		Img frameDisplayFHT = new Img(superFrame.getFrame().getSrc().clone(), false);
		vStrip = 0;
		for (Function<Double, Double> f : approxVFHTFunctions) {
			for (int k = hStep; k + hStep <= binarized.height(); k += hStep) {
				double angle = (f.apply((double) k) - minAngle) / 180 * Math.PI;
				Imgproc.line(frameDisplayFHT.getSrc(), new Point((vStrip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 6, k - Math.sin(angle) * stripWidth / 6),
						new Point((vStrip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 6, k + Math.sin(angle) * stripWidth / 6), new Scalar(0, 255, 0), 2);
				angle = interpolatorFHT.interpolateVerticals((vStrip + 1) * stripWidth / 2, k);
				Imgproc.line(frameDisplayFHT.getSrc(), new Point((vStrip + 1) * stripWidth / 2 - Math.cos(angle) * stripWidth / 6, k - Math.sin(angle) * stripWidth / 6),
						new Point((vStrip + 1) * stripWidth / 2 + Math.cos(angle) * stripWidth / 6, k + Math.sin(angle) * stripWidth / 6), new Scalar(255, 0, 0), 2);
			}
			vStrip++;
		}
		hStrip = 0;
		for (Function<Double, Double> f : approxHFHTFunctions) {
			for (int k = vStep; k + vStep <= binarized.width(); k += vStep) {
				double angle = (90 + minAngle - (f.apply((double) k))) / 180 * Math.PI;
				Imgproc.line(frameDisplayFHT.getSrc(), new Point(k - Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 - Math.sin(angle) * stripHeight / 6),
						new Point(k + Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 + Math.sin(angle) * stripHeight / 6), new Scalar(0, 0, 255), 2);
				angle = interpolatorFHT.interpolateHorizontals(k, (hStrip + 1) * stripHeight / 2);
				Imgproc.line(frameDisplayFHT.getSrc(), new Point(k - Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 - Math.sin(angle) * stripHeight / 6),
						new Point(k + Math.cos(angle) * stripHeight / 6, (hStrip + 1) * stripHeight / 2 + Math.sin(angle) * stripHeight / 6), new Scalar(255, 0, 0), 2);

			}
			hStrip++;
		}
		images[2] = frameDisplayFHT.toJfxImage();

		ref = trace("Display lines", ref);

		ref = trace("Display lines", ref);

		MeshGrid meshGrid = new MeshGrid(new Size(16, 9), interpolator, 20, 20, superFrame.getFrame().getSrc());
		meshGrid.build();
		MeshGrid meshGridFHT = new MeshGrid(new Size(16, 9), interpolatorFHT, 20, 20, superFrame.getFrame().getSrc());
		meshGridFHT.build();
		ref = trace("Build mesh", ref);

		images[4] = new Img(meshGrid.drawOnCopy(new Scalar(0, 255, 0)), false).toJfxImage();
		images[5] = new Img(meshGridFHT.drawOnCopy(new Scalar(0, 255, 0)), false).toJfxImage();
		ref = trace("Draw mesh", ref);

		Img dewarp = new Img(meshGrid.dewarp());
		Img dewarpFHT = new Img(meshGridFHT.dewarp());
		images[7] = dewarp.toJfxImage();
		images[8] = dewarpFHT.toJfxImage();
		ref = trace("Dewarp", ref);

		// images[7] = new Img(RadonTransform.estimateBaselines(superFrame.getFrame().getSrc(), 0), false).toJfxImage();

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
