package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator;
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;

public class Segment implements Comparable<Segment> {
	final OrientedPoint op1;
	final OrientedPoint op2;
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

	public static List<List<Segment>>[] connect(List<List<OrientedPoint>[]> trajects, double w, double maxDistanceCoeff, boolean vertical) {
		List<List<Segment>> topResult = new ArrayList<>();
		List<List<Segment>> bottomResult = new ArrayList<>();
		for (int i = 0; i < trajects.size() - 1; i++) {
			List<Segment>[] connectStrips = connectStrips(trajects.get(i), trajects.get(i + 1), w, maxDistanceCoeff, vertical);
			topResult.add(connectStrips[0]);
			bottomResult.add(connectStrips[1]);
		}
		return new List[] { topResult, bottomResult };
	}

	private static List<Segment>[] connectStrips(List<OrientedPoint>[] traject1, List<OrientedPoint>[] traject2, double w, double maxDistanceCoeff, boolean vertical) {
		List<Segment> topResult = new ArrayList<>();
		final List<Segment> topSortedFilteredEdges = new ArrayList<>();
		traject1[0].forEach(step1 -> traject2[0].forEach(step2 -> topSortedFilteredEdges.add(new Segment(step1, step2, w, vertical))));

		topSortedFilteredEdges.removeIf(edge -> edge.getDistance() > maxDistanceCoeff * w);
		Collections.sort(topSortedFilteredEdges);
		while (!topSortedFilteredEdges.isEmpty()) {
			Segment selected = topSortedFilteredEdges.get(0);
			topResult.add(selected);
			topSortedFilteredEdges.removeIf(selected::invalidate);
		}

		List<Segment> bottomResult = new ArrayList<>();
		final List<Segment> bottomSortedFilteredEdges = new ArrayList<>();
		traject1[1].forEach(step1 -> traject2[1].forEach(step2 -> bottomSortedFilteredEdges.add(new Segment(step1, step2, w, vertical))));
		bottomSortedFilteredEdges.removeIf(edge -> edge.getDistance() > maxDistanceCoeff * w);
		Collections.sort(bottomSortedFilteredEdges);
		while (!bottomSortedFilteredEdges.isEmpty()) {
			Segment selected = bottomSortedFilteredEdges.get(0);
			bottomResult.add(selected);
			bottomSortedFilteredEdges.removeIf(selected::invalidate);
		}
		return new List[] { topResult, bottomResult };
	}

	public static void displayHorizontalOps(List<List<Segment>> horizontalSegments, Mat img, double vStep, double hStep, Scalar color) {
		horizontalSegments.stream().flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).forEach(op -> op.displayHorizontalOp(img, vStep, hStep, color));
	}

	public static void displayVerticalOps(List<List<Segment>> verticalSegments, Mat img, double vStep, double hStep, Scalar color) {
		verticalSegments.stream().flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).forEach(op -> op.displayVerticalOp(img, vStep, hStep, color));
	}

	public static List<PolynomialSplineFunction>[] toSplines(List<List<Segment>>[] connectedEdges, boolean vertical) {
		List<List<Segment>> topConnectedEdges = connectedEdges[0];
		List<List<Segment>> bottomConnectedEdges = connectedEdges[1];
		return new List[] { toSplines(topConnectedEdges, vertical), toSplines(bottomConnectedEdges, vertical) };
	}

	private static List<PolynomialSplineFunction> toSplines(List<List<Segment>> connectedEdges, boolean vertical) {
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

	private static PolynomialSplineFunction toSpline(List<OrientedPoint> orientedPoints, boolean vertical) {
		return new LinearInterpolator().interpolate(orientedPoints.stream().mapToDouble(op -> vertical ? op.center.y : op.center.x).toArray(), orientedPoints.stream().mapToDouble(op -> vertical ? op.center.x : op.center.y).toArray());
	}
}