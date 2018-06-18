package org.genericsystem.cv.application;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.opencv.core.Core;
import org.opencv.core.Mat;

public class FHTManager {

	private final Img binarized;
	private final Img transposedBinarized;

	private final double stripWidth;
	private final double stripHeight;
	private final double vStep;
	private final double hStep;
	private final int vStripsNumber;
	private final int hStripsNumber;

	public FHTManager(Img binarized, double vRecover, double hRecover) {
		this.binarized = binarized;
		vStripsNumber = (int) ((16d / 3 - vRecover + 1) / (1 - vRecover));
		stripWidth = (binarized.width() / (vStripsNumber * (1 - vRecover) + vRecover - 1));
		vStep = ((1 - vRecover) * stripWidth);
		this.transposedBinarized = binarized.transpose();
		hStripsNumber = (int) ((9d / 3 - hRecover + 1) / (1 - hRecover));
		stripHeight = (binarized.height() / (hStripsNumber * (1 - hRecover) + hRecover - 1));
		hStep = ((1 - hRecover) * stripHeight);
	}

	public Img dewarp(Mat frame) {
		List<Mat> vStrips = FHT.extractStrips(binarized.getSrc(), vStripsNumber, stripWidth, vStep);
		List<Mat> hStrips = FHT.extractStrips(transposedBinarized.getSrc(), hStripsNumber, stripHeight, hStep);
		List<Mat> vHoughs = vStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList());
		vHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		hHoughs.forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		List<List<TrajectStep>> vHoughTrajs = vHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, 21, -0.08)).collect(Collectors.toList());
		List<List<TrajectStep>> hHoughTrajs = hHoughs.stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, 21, -0.08)).collect(Collectors.toList());
		// vHoughTrajs = StripTractor.optimize(vHoughs, 21, -0.08, -100, vHoughTrajs, vStep);
		// hHoughTrajs = StripTractor.optimize(hHoughs, 21, -0.08, -100, hHoughTrajs, hStep);
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
}
