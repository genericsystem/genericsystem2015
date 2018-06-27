package org.genericsystem.cv.application;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.opencv.core.Core;
import org.opencv.core.Mat;

import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleIntegerProperty;

public class FHTManager {

	// private Property<Double> stripWidth = new SimpleObjectProperty<>();
	// private Property<Double> stripHeight = new SimpleObjectProperty<>();
	// private Property<Double> vStep = new SimpleObjectProperty<>();
	// private Property<Double> hStep = new SimpleObjectProperty<>();
	// private Property<Integer> vStripsNumber = new SimpleObjectProperty<>();
	// private Property<Integer> hStripsNumber = new SimpleObjectProperty<>();

	private IntegerProperty vBlurSize = new SimpleIntegerProperty(21);
	private IntegerProperty hBlurSize = new SimpleIntegerProperty(21);
	private DoubleProperty vNeighbourPenality = new SimpleDoubleProperty(-100);
	private DoubleProperty hNeighbourPenality = new SimpleDoubleProperty(-100);
	private DoubleProperty vAnglePenality = new SimpleDoubleProperty(-0.08);
	private DoubleProperty hAnglePenality = new SimpleDoubleProperty(-0.08);

	public FHTManager() {
		vBlurSize.addListener((a, b, newValue) -> System.out.println("ZZZZZZZZZZZZZZZZZZZZZZZ" + newValue));
		hBlurSize.addListener((a, b, newValue) -> System.out.println("YYYYYYYYYYYYYYYYYYYYYYY" + newValue));
		vNeighbourPenality.addListener((a, b, newValue) -> System.out.println("ZZZZZZZZZZZZZZZZZZZZZZZ" + newValue));
		hNeighbourPenality.addListener((a, b, newValue) -> System.out.println("YYYYYYYYYYYYYYYYYYYYYYY" + newValue));
		vAnglePenality.addListener((a, b, newValue) -> System.out.println("ZZZZZZZZZZZZZZZZZZZZZZZ" + newValue));
		hAnglePenality.addListener((a, b, newValue) -> System.out.println("YYYYYYYYYYYYYYYYYYYYYYY" + newValue));
	}

	public Img dewarp(Mat frame, Mat binarized, double vRecover, double hRecover) {
		int vStripsNumber = (int) ((16d / 3 - vRecover + 1) / (1 - vRecover));
		double stripWidth = (binarized.width() / (vStripsNumber * (1 - vRecover) + vRecover - 1));
		double vStep = ((1 - vRecover) * stripWidth);
		Mat transposedBinarized = new Mat();
		Core.transpose(binarized, transposedBinarized);
		int hStripsNumber = (int) ((9d / 3 - hRecover + 1) / (1 - hRecover));
		double stripHeight = (binarized.height() / (hStripsNumber * (1 - hRecover) + hRecover - 1));
		double hStep = ((1 - hRecover) * stripHeight);
		List<Mat> vStrips = FHT.extractStrips(binarized, vStripsNumber, stripWidth, vStep);
		List<Mat> hStrips = FHT.extractStrips(transposedBinarized, hStripsNumber, stripHeight, hStep);
		List<Mat> vHoughs = vStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());
		vHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		hHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		List<List<TrajectStep>> vHoughTrajs = vHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, vBlurSize.getValue(), vAnglePenality.getValue())).collect(Collectors.toList());
		List<List<TrajectStep>> hHoughTrajs = hHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, hBlurSize.getValue(), hAnglePenality.getValue())).collect(Collectors.toList());
		// vHoughTrajs = StripTractor.optimize(vHoughs, vBlurSize.getValue(), vAnglePenality.getValue(), vNeighbourPenality.get(), vHoughTrajs, vStep);
		// hHoughTrajs = StripTractor.optimize(hHoughs, hBlurSize.getValue(), hAnglePenality.getValue(), hNeighbourPenality.get(), hHoughTrajs, hStep);
		List<List<OrientedPoint>[]> fhtHorizontals = ProjectionLines.toHorizontalsOrientedPoints(vHoughTrajs, vStep, 0.5, 0.05);
		List<List<OrientedPoint>[]> fhtVerticals = ProjectionLines.toVerticalsOrientedPoints(hHoughTrajs, hStep, 0.5, 0.05);
		List<List<Segment>>[] horizontalSegments = Segment.connect(fhtHorizontals, vStep, 0.05, false);
		List<List<Segment>>[] verticalSegments = Segment.connect(fhtVerticals, hStep, 0.05, true);
		List<PolynomialSplineFunction>[] horizontalSplines = Segment.toSplines(horizontalSegments, false);
		List<PolynomialSplineFunction>[] verticalSplines = Segment.toSplines(verticalSegments, true);
		List<OrientedPoint> flatHorizontalSegments = Stream.of(horizontalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
		List<OrientedPoint> flatVerticalSegments = Stream.of(verticalSegments).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList());
		GeneralInterpolator interpolatorFHT = new GeneralInterpolator(flatHorizontalSegments, flatVerticalSegments, 4, 0.0001);
		SplineInterpolator superInterpolator = new SplineInterpolator(interpolatorFHT, horizontalSplines, verticalSplines);
		MeshManager meshManager = new MeshManager(6, 4, superInterpolator, frame);
		return new Img(meshManager.dewarp3D());
	}

	public IntegerProperty getvBlurSize() {
		return vBlurSize;
	}

	public IntegerProperty gethBlurSize() {
		return hBlurSize;
	}

	public DoubleProperty gethNeighbourPenality() {
		return hNeighbourPenality;
	}

	public DoubleProperty getvNeighbourPenality() {
		return vNeighbourPenality;
	}

	public DoubleProperty getvAnglePenality() {
		return vAnglePenality;
	}

	public DoubleProperty gethAnglePenality() {
		return hAnglePenality;
	}

}
