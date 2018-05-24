package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator;
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Layout;
import org.opencv.core.Core;
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
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };
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

		images[0] = superFrame.getFrame().toJfxImage();

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 5);// .dilate(Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 2)));// .canny(20, 80);
		// Mat frameClone = superFrame.getFrame().getSrc().clone();
		// DirectionalEnhancer.drawFilteredLines(binarized.getSrc(), DirectionalEnhancer.getLines(frameClone));

		images[1] = binarized.toJfxImage();

		if (frameCount < 30)
			return images;
		Img transposedBinarized = binarized.transpose();
		ref = trace("Binarization", ref);

		double vRecover = 0.5;
		int vStripsNumber = 16;
		double stripWidth = (binarized.width() / (vStripsNumber * (1 - vRecover) + vRecover));
		double vStep = ((1 - vRecover) * stripWidth);
		System.out.println(vStripsNumber + " verticals strips with width : " + stripWidth + " each step : " + vStep);

		double hRecover = 0.5;
		int hStripsNumber = 9;
		double stripHeight = (binarized.height() / (hStripsNumber * (1 - hRecover) + hRecover));
		double hStep = ((1 - hRecover) * stripHeight);
		System.out.println(hStripsNumber + " horizontal strips with width : " + stripHeight + " each step : " + hStep);

		List<Mat> vStrips = RadonTransform.extractStrips(binarized.getSrc(), vStripsNumber, stripWidth, vStep);
		List<Mat> hStrips = RadonTransform.extractStrips(transposedBinarized.getSrc(), hStripsNumber, stripHeight, hStep);
		ref = trace("Extract strips", ref);

		List<Mat> vHoughs = vStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());

		vHoughs.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		hHoughs.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		ref = trace("Compute FHT", ref);

		List<List<HoughTrajectStep>> vHoughTrajs = vHoughs.stream().map(projectionMap -> RadonTransform.bestTrajectFHT(projectionMap, 11, -0.2)).collect(Collectors.toList());
		List<List<HoughTrajectStep>> hHoughTrajs = hHoughs.stream().map(projectionMap -> RadonTransform.bestTrajectFHT(projectionMap, 11, -0.2)).collect(Collectors.toList());
		ref = trace("Compute trajects", ref);

		List<List<OrientedPoint>[]> fhtHorizontals = new ArrayList<>();
		for (int vStripIndex = 0; vStripIndex < vHoughTrajs.size(); vStripIndex++)
			fhtHorizontals.add(RadonTransform.toHorizontalOrientedPoints(vHoughTrajs.get(vStripIndex), (vStripIndex + 1) * vStep, 0.3, 0.1));
		List<List<OrientedPoint>[]> fhtVerticals = new ArrayList<>();
		for (int hStrip = 0; hStrip < hHoughTrajs.size(); hStrip++)
			fhtVerticals.add(RadonTransform.toVerticalOrientedPoints(hHoughTrajs.get(hStrip), (hStrip + 1) * hStep, 0.3, 0.1));

		List<List<Segment>>[] horizontalSegments = connect(fhtHorizontals, hStep, 2, false);
		List<PolynomialSplineFunction>[] horizontalSplines = toSplines(horizontalSegments, false);
		List<List<Segment>>[] verticalSegments = connect(fhtVerticals, vStep, 2, true);
		List<PolynomialSplineFunction>[] verticalSplines = toSplines(verticalSegments, true);

		Img splineDisplay = new Img(superFrame.getFrame().getSrc().clone(), false);
		for (PolynomialSplineFunction spline : horizontalSplines[0])
			for (double x = spline.getKnots()[0]; x < spline.getKnots()[spline.getKnots().length - 1]; x++)
				splineDisplay.getSrc().put((int) Math.round(spline.value(x)), (int) x, 0, 255, 0);
		for (PolynomialSplineFunction spline : horizontalSplines[1])
			for (double x = spline.getKnots()[0]; x < spline.getKnots()[spline.getKnots().length - 1]; x++)
				splineDisplay.getSrc().put((int) Math.round(spline.value(x)), (int) x, 0, 0, 255);

		for (PolynomialSplineFunction spline : verticalSplines[0])
			for (double y = spline.getKnots()[0]; y < spline.getKnots()[spline.getKnots().length - 1]; y++)
				splineDisplay.getSrc().put((int) y, (int) Math.round(spline.value(y)), 255, 255, 0);
		for (PolynomialSplineFunction spline : verticalSplines[1])
			for (double y = spline.getKnots()[0]; y < spline.getKnots()[spline.getKnots().length - 1]; y++)
				splineDisplay.getSrc().put((int) y, (int) Math.round(spline.value(y)), 255, 0, 255);

		images[2] = splineDisplay.toJfxImage();
		ref = trace("Display splines", ref);

		List<OrientedPoint> flatHorizontalSegments = Stream.of(horizontalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
		List<OrientedPoint> flatVerticalSegments = Stream.of(verticalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());

		List<OrientedPoint> flatFhtVerticals = fhtVerticals.stream().flatMap(h -> Stream.of(h)).flatMap(h -> h.stream()).collect(Collectors.toList());
		List<OrientedPoint> flatFhtHorizontals = fhtHorizontals.stream().flatMap(h -> Stream.of(h)).flatMap(h -> h.stream()).collect(Collectors.toList());
		GeneralInterpolator interpolatorFHT = new GeneralInterpolator(flatHorizontalSegments, flatVerticalSegments, 4, 0.0001);

		ref = trace("Prepare interpolator", ref);

		Img frameDisplayFHT = new Img(superFrame.getFrame().getSrc().clone(), false);

		// for (OrientedPoint op : flatFhtVerticals) {
		// double angle = op.angle + Math.PI / 2;
		// Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(angle) * stripHeight / 2 * op.strenght),
		// new Point(op.center.x + Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(angle) * stripHeight / 2 * op.strenght), new Scalar(0, 0, 255), 1);
		// angle = interpolatorFHT.interpolateVerticals(op.center.x, op.center.y) + Math.PI / 2;
		// Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(angle) * stripHeight / 2 * op.strenght),
		// new Point(op.center.x + Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(angle) * stripHeight / 2 * op.strenght), new Scalar(255, 0, 0), 1);
		// }

		// for (OrientedPoint op : flatFhtHorizontals) {
		// Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(op.angle) * stripHeight / 2 * op.strenght),
		// new Point(op.center.x + Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(op.angle) * stripHeight / 2 * op.strenght), new Scalar(0, 0, 255), 1);
		// double angle = interpolatorFHT.interpolateHorizontals(op.center.x, op.center.y);
		// Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(angle) * stripHeight / 2 * op.strenght),
		// new Point(op.center.x + Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(angle) * stripHeight / 2 * op.strenght), new Scalar(255, 0, 0), 1);
		//
		// }

		for (OrientedPoint op : horizontalSegments[0].stream().flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList())) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(op.angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x + Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(op.angle) * stripHeight / 2 * op.strenght), new Scalar(0, 255, 0), 1);

		}
		for (OrientedPoint op : horizontalSegments[1].stream().flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList())) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(op.angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x + Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(op.angle) * stripHeight / 2 * op.strenght), new Scalar(0, 0, 255), 1);

		}

		for (OrientedPoint op : verticalSegments[0].stream().flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList())) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x + Math.sin(op.angle) * stripWidth / 2 * op.strenght, op.center.y - Math.cos(op.angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x - Math.sin(op.angle) * stripWidth / 2 * op.strenght, op.center.y + Math.cos(op.angle) * stripHeight / 2 * op.strenght), new Scalar(255, 255, 0), 1);

		}
		for (OrientedPoint op : verticalSegments[1].stream().flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList())) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x + Math.sin(op.angle) * stripWidth / 2 * op.strenght, op.center.y - Math.cos(op.angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x - Math.sin(op.angle) * stripWidth / 2 * op.strenght, op.center.y + Math.cos(op.angle) * stripHeight / 2 * op.strenght), new Scalar(255, 0, 255), 1);

		}
		images[3] = frameDisplayFHT.toJfxImage();
		ref = trace("Display lines", ref);

		MeshManager meshManager = new MeshManager(6, 4, interpolatorFHT, superFrame.getFrame().getSrc());
		ref = trace("Build mesh", ref);

		images[4] = new Img(meshManager.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("Draw mesh", ref);

		Img dewarpFHT = new Img(meshManager.dewarp());
		images[5] = dewarpFHT.toJfxImage();
		ref = trace("Dewarp", ref);

		Img dewarpedBinarized2 = dewarpFHT.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[6] = dewarpedBinarized2.toJfxImage();
		ref = trace("Binarize dewarp", ref);

		Layout layout = dewarpedBinarized2.buildLayout(new Size(2, 0.0), new Size(0.01, 0.01), 8);
		layout.draw(dewarpFHT, new Scalar(255, 0, 0), new Scalar(0, 0, 255), 1, 2);
		images[7] = dewarpFHT.toJfxImage();
		ref = trace("Layout", ref);

		images[8] = new Img(meshManager.draw3Dsurface(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("3D surface / svd", ref);

		Img dewarpFHT3D = new Img(meshManager.dewarp3D());
		images[9] = dewarpFHT3D.toJfxImage();
		ref = trace("Dewarp 3D", ref);

		Img binarized3D = dewarpFHT3D.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[10] = binarized3D.toJfxImage();
		ref = trace("Binarize dewarp 3D", ref);

		Layout layout3D = binarized3D.buildLayout(new Size(2, 0.0), new Size(0.01, 0.01), 8);
		layout3D.draw(dewarpFHT3D, new Scalar(255, 0, 0), new Scalar(0, 0, 255), 1, 2);
		images[11] = dewarpFHT3D.toJfxImage();
		ref = trace("Layout 3D", ref);

		meshManager.recomputeGrid();
		images[12] = new Img(meshManager.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("Redraw mesh", ref);

		images[13] = new Img(meshManager.draw3Dsurface(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("Redraw mesh", ref);

		images[14] = new Img(meshManager.dewarp3D(), false).toJfxImage();
		ref = trace("Redraw mesh", ref);

		return images;

	}

	private List<PolynomialSplineFunction>[] toSplines(List<List<Segment>>[] connectedEdges, boolean vertical) {
		List<List<Segment>> topConnectedEdges = connectedEdges[0];
		List<List<Segment>> bottomConnectedEdges = connectedEdges[1];
		return new List[] { toSplines(topConnectedEdges, vertical), toSplines(bottomConnectedEdges, vertical) };
	}

	private List<PolynomialSplineFunction> toSplines(List<List<Segment>> connectedEdges, boolean vertical) {
		List<List<OrientedPoint>> orientedPointsSuperList = new ArrayList<>();
		for (List<Segment> segments : connectedEdges) {
			LOOP1: for (Segment segment : segments) {
				for (List<OrientedPoint> opList : orientedPointsSuperList)
					if (segment.op1.equals(opList.get(opList.size() - 1))) {
						opList.add(segment.op2);
						continue LOOP1;
					}
				orientedPointsSuperList.add(new ArrayList<>(Arrays.asList(segment.op1, segment.op2)));
			}
		}
		return orientedPointsSuperList.stream().map(ops -> toSpline(ops, vertical)).collect(Collectors.toList());

	}

	private PolynomialSplineFunction toSpline(List<OrientedPoint> orientedPoints, boolean vertical) {
		return new LinearInterpolator().interpolate(orientedPoints.stream().mapToDouble(op -> vertical ? op.center.y : op.center.x).toArray(), orientedPoints.stream().mapToDouble(op -> vertical ? op.center.x : op.center.y).toArray());
	}

	private List<List<Segment>>[] connect(List<List<OrientedPoint>[]> trajects, double w, double maxDistance, boolean vertical) {
		List<List<Segment>> topResult = new ArrayList<>();
		List<List<Segment>> bottomResult = new ArrayList<>();
		for (int i = 0; i < trajects.size() - 1; i++) {
			List<Segment>[] connectStrips = connectStrips(trajects.get(i), trajects.get(i + 1), w, maxDistance, vertical);
			topResult.add(connectStrips[0]);
			bottomResult.add(connectStrips[1]);
		}
		return new List[] { topResult, bottomResult };
	}

	private List<Segment>[] connectStrips(List<OrientedPoint>[] traject1, List<OrientedPoint>[] traject2, double w, double maxDistance, boolean vertical) {
		List<Segment> topResult = new ArrayList<>();
		final List<Segment> topSortedFilteredEdges = new ArrayList<>();
		traject1[0].forEach(step1 -> traject2[0].forEach(step2 -> topSortedFilteredEdges.add(new Segment(step1, step2, w, vertical))));

		topSortedFilteredEdges.removeIf(edge -> edge.getDistance() > maxDistance);
		Collections.sort(topSortedFilteredEdges);
		while (!topSortedFilteredEdges.isEmpty()) {
			Segment selected = topSortedFilteredEdges.get(0);
			topResult.add(selected);
			topSortedFilteredEdges.removeIf(selected::invalidate);
		}

		List<Segment> bottomResult = new ArrayList<>();
		final List<Segment> bottomSortedFilteredEdges = new ArrayList<>();
		traject1[1].forEach(step1 -> traject2[1].forEach(step2 -> bottomSortedFilteredEdges.add(new Segment(step1, step2, w, vertical))));
		bottomSortedFilteredEdges.removeIf(edge -> edge.getDistance() > maxDistance);
		Collections.sort(bottomSortedFilteredEdges);
		while (!bottomSortedFilteredEdges.isEmpty()) {
			Segment selected = bottomSortedFilteredEdges.get(0);
			bottomResult.add(selected);
			bottomSortedFilteredEdges.removeIf(selected::invalidate);
		}
		return new List[] { topResult, bottomResult };
	}

	public static class Segment implements Comparable<Segment> {
		private final OrientedPoint op1;
		private final OrientedPoint op2;
		private final double distance;

		public Segment(OrientedPoint op1, OrientedPoint op2, double w, boolean vertical) {
			this.op1 = op1;
			this.op2 = op2;
			this.distance = Math.abs((w * op1.derivative / 2 + (vertical ? op1.center.x : op1.center.y)) - ((vertical ? op2.center.x : op2.center.y) - w * op2.derivative / 2));
		}

		public boolean invalidate(Segment selected) {
			return op1.equals(selected.op1) || op2.equals(selected.op2);
		}

		public double getDistance() {
			return distance;
		}

		@Override
		public int compareTo(Segment e) {
			return Double.compare(distance, e.distance);
		}
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
