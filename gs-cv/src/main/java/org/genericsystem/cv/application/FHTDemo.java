package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Layout;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class FHTDemo extends AbstractApp {

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
			frameCount++;
		}
		Image[] images = new Image[20];
		long ref = System.currentTimeMillis();
		images[0] = superFrame.getFrame().toJfxImage();

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 5);// .dilate(Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 2)));// .canny(20, 80);
		images[1] = binarized.toJfxImage();
		if (frameCount < 30)
			return images;
		Img transposedBinarized = binarized.transpose();
		ref = trace("Binarization", ref);

		double vRecover = 0.80;
		int vStripsNumber = (int) ((16d / 5 - vRecover + 1) / (1 - vRecover));
		double stripWidth = (binarized.width() / (vStripsNumber * (1 - vRecover) + vRecover - 1));
		double vStep = ((1 - vRecover) * stripWidth);
		double minVerticalAccuracy = 180 * (Math.atan(1 / (stripWidth - 1))) / Math.PI;
		System.out.println(vStripsNumber + " verticals strips with width : " + stripWidth + " each step : " + vStep + " min accuracy : " + minVerticalAccuracy);

		double hRecover = 0.80;
		int hStripsNumber = (int) ((9d / 5 - hRecover + 1) / (1 - hRecover));
		double stripHeight = (binarized.height() / (hStripsNumber * (1 - hRecover) + hRecover - 1));
		double hStep = ((1 - hRecover) * stripHeight);
		double minHorizontalAccuracy = 180 * (Math.atan(1 / (stripHeight - 1))) / Math.PI;
		System.out.println(hStripsNumber + " horizontal strips with width : " + stripHeight + " each step : " + hStep + " min accuracy : " + minHorizontalAccuracy);

		Mat enlargedBinarized = new Mat();
		Core.copyMakeBorder(binarized.getSrc(), enlargedBinarized, 0, 0, (int) Math.round(stripWidth / 2), (int) Math.round(stripWidth / 2), Core.BORDER_CONSTANT, new Scalar(0));

		Mat enlargedTransposedBinarized = new Mat();
		Core.copyMakeBorder(transposedBinarized.getSrc(), enlargedTransposedBinarized, 0, 0, (int) Math.round(stripHeight / 2), (int) Math.round(stripHeight / 2), Core.BORDER_CONSTANT, new Scalar(0));

		List<Mat> vStrips = FHT.extractStrips(enlargedBinarized, vStripsNumber, stripWidth, vStep);
		List<Mat> hStrips = FHT.extractStrips(enlargedTransposedBinarized, hStripsNumber, stripHeight, hStep);
		ref = trace("Extract strips", ref);

		List<Mat> vHoughs = vStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());

		vHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		hHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		ref = trace("Compute FHT", ref);

		List<List<TrajectStep>> vHoughTrajs = vHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, 21, -0.08)).collect(Collectors.toList());
		List<List<TrajectStep>> hHoughTrajs = hHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, 21, -0.08)).collect(Collectors.toList());
		ref = trace("Compute trajects", ref);

		List<List<TrajectStep>> vInfluencedTrajs = StripTractor.optimize(vHoughs, 21, -0.08, -1000, vHoughTrajs, vStep);
		List<List<TrajectStep>> hInfluencedTrajs = StripTractor.optimize(hHoughs, 21, -0.08, -1000, hHoughTrajs, hStep);

		List<List<OrientedPoint>[]> fhtHorizontals = ProjectionLines.toHorizontalsOrientedPoints(vInfluencedTrajs, vStep, 0.5, 0.05);
		List<List<OrientedPoint>[]> fhtVerticals = ProjectionLines.toVerticalsOrientedPoints(hInfluencedTrajs, hStep, 0.5, 0.05);

		List<List<Segment>>[] horizontalSegments = Segment.connect(fhtHorizontals, vStep, 0.05, false);
		List<List<Segment>>[] verticalSegments = Segment.connect(fhtVerticals, hStep, 0.05, true);

		Img frameDisplayFHT = new Img(superFrame.getFrame().getSrc().clone(), false);
		Segment.displayHorizontalOps(horizontalSegments[0], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(0, 255, 0));
		Segment.displayHorizontalOps(horizontalSegments[1], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(0, 0, 255));
		Segment.displayVerticalOps(verticalSegments[0], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(255, 255, 0));
		Segment.displayVerticalOps(verticalSegments[1], frameDisplayFHT.getSrc(), vStep, hStep, new Scalar(255, 0, 255));

		images[2] = frameDisplayFHT.toJfxImage();
		ref = trace("Display lines", ref);

		List<PolynomialSplineFunction>[] horizontalSplines = Segment.toSplines(horizontalSegments, false);
		List<PolynomialSplineFunction>[] verticalSplines = Segment.toSplines(verticalSegments, true);
		Img splineDisplay = new Img(superFrame.getFrame().getSrc().clone(), false);
		FHT.displayHSplines(horizontalSplines[0], splineDisplay.getSrc(), 0, 255, 0);
		FHT.displayHSplines(horizontalSplines[1], splineDisplay.getSrc(), 0, 0, 255);
		FHT.displayVSplines(verticalSplines[0], splineDisplay.getSrc(), 255, 255, 0);
		FHT.displayVSplines(verticalSplines[1], splineDisplay.getSrc(), 255, 0, 255);
		images[3] = splineDisplay.toJfxImage();
		ref = trace("Display splines", ref);

		List<OrientedPoint> flatHorizontalSegments = Stream.of(horizontalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
		List<OrientedPoint> flatVerticalSegments = Stream.of(verticalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
		GeneralInterpolator interpolatorFHT = new GeneralInterpolator(flatHorizontalSegments, flatVerticalSegments, 4, 0.0001);
		SplineInterpolator superInterpolator = new SplineInterpolator(interpolatorFHT, horizontalSplines, verticalSplines);
		ref = trace("Prepare interpolator", ref);

		MeshManager meshManager = new MeshManager(6, 4, superInterpolator, superFrame.getFrame().getSrc());
		images[4] = new Img(meshManager.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("Build and draw mesh", ref);

		images[5] = new Img(meshManager.draw3Dsurface(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("3D surface / svd", ref);

		Img dewarpFHT3D = new Img(meshManager.dewarp3D());
		images[6] = dewarpFHT3D.toJfxImage();
		ref = trace("Dewarp 3D", ref);

		Img binarized3D = dewarpFHT3D.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[7] = binarized3D.toJfxImage();
		ref = trace("Binarize dewarp 3D", ref);

		Layout layout3D = binarized3D.buildLayout(new Size(2, 0.0), new Size(0.001, 0.001), 8);
		layout3D.draw(dewarpFHT3D, new Scalar(255, 0, 0), new Scalar(0, 0, 255), 0, -1);
		images[8] = dewarpFHT3D.toJfxImage();
		ref = trace("Layout 3D", ref);

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
