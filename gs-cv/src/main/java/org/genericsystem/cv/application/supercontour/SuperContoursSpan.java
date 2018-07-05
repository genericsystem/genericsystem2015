package org.genericsystem.cv.application.supercontour;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator;
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class SuperContoursSpan implements Comparable<SuperContoursSpan> {

	private List<SuperContour> contours = new ArrayList<>();
	private Function<Double, Double> approx;

	public void add(SuperContour superContour) {
		contours.add(superContour);
	}

	public List<SuperContour> getContours() {
		return contours;
	}

	// BiFunction<Double, double[], Double> f = (x, params) -> params[0] + params[1] * x + params[2] * x * x;
	// BiFunction<double[], double[], Double> error = (xy, params) -> f.apply(xy[0], params) - xy[1];

	public Function<Double, Double> getApprox() {
		return approx != null ? approx : (approx = computeApprox());
	}

	private Function<Double, Double> computeApprox() {
		// double[] params = new LevenbergImpl<>(error, contours.stream().map(sc -> new double[] { sc.center.x, sc.center.y }).collect(Collectors.toList()), new double[] { 0, 0, 0 }).getParams();
		// return x -> f.apply(x, params);
		PolynomialSplineFunction psf = new LinearInterpolator().interpolate(contours.stream().mapToDouble(sc -> sc.center.x).toArray(), contours.stream().mapToDouble(sc -> sc.center.y).toArray());
		return x -> psf.isValidPoint(x) ? psf.value(x) : null;
	}

	@Override
	public int compareTo(SuperContoursSpan span) {
		return Double.compare(getContours().stream().mapToDouble(c -> c.center.y).average().getAsDouble(), span.getContours().stream().mapToDouble(c -> c.center.y).average().getAsDouble());
	}

	public static List<SuperContour> detectSuperContours(double minArea, Mat mask) {
		List<MatOfPoint> contours = new ArrayList<>();
		Mat hierarchy = new Mat();
		Imgproc.findContours(mask, contours, hierarchy, Imgproc.RETR_TREE, Imgproc.CHAIN_APPROX_NONE);
		List<SuperContour> result = new ArrayList<>();
		int row = 0;
		for (MatOfPoint contour : contours) {
			double[] indexes = hierarchy.get(0, row);
			double fatherIndex = indexes[3];
			// double prevBrotherIndex = indexes[1];
			// double nextBrotherIndex = indexes[0];
			// double childIndex = indexes[2];
			MatOfPoint fatherWrapper = fatherIndex != -1 ? contours.get((int) fatherIndex) : null;
			if (Imgproc.contourArea(contour) > minArea && Imgproc.boundingRect(contour).area() < 10000)
				if (fatherWrapper != null) {
					if (countWhitePixels(contour, Imgproc.boundingRect(contour), mask) != 0) {
						result.add(new SuperContour(contour));
					}
				} else {
					result.add(new SuperContour(contour));
				}
			row++;
		}
		// Collections.sort(result);

		return result;
	}

	public static class Edge implements Comparable<Edge> {

		private final double score;
		private final SuperContour c1;
		private final SuperContour c2;

		public Edge(double score, SuperContour c1, SuperContour c2) {
			this.c1 = c1;
			this.c2 = c2;
			this.score = score;
		}

		@Override
		public int compareTo(Edge edge) {
			return Double.compare(score, edge.getScore());
		}

		private double getScore() {
			return score;
		}

		public SuperContour getC1() {
			return c1;
		}

		public SuperContour getC2() {
			return c2;
		}

	}

	public static int countWhitePixels(MatOfPoint contour, Rect rect, Mat img) {
		Mat mask = Mat.zeros(rect.size(), CvType.CV_8UC1);
		Imgproc.drawContours(mask, Arrays.asList(contour), 0, new Scalar(255), -1, Imgproc.LINE_8, new Mat(), Integer.MAX_VALUE, new Point(-rect.tl().x, -rect.tl().y));
		Imgproc.drawContours(mask, Arrays.asList(contour), 0, new Scalar(0), 1, Imgproc.LINE_8, new Mat(), Integer.MAX_VALUE, new Point(-rect.tl().x, -rect.tl().y));
		int white = 0;
		for (int row = 0; row < mask.rows(); row++)
			for (int col = 0; col < mask.cols(); col++)
				if (mask.get(row, col)[0] != 0 && img.get(row + (int) rect.tl().y, col + (int) rect.tl().x)[0] != 0)
					white++;
		return white;
	}

	public static List<SuperContoursSpan> assembleContours(List<SuperContour> superContours, Predicate<SuperContour> contoursFilter, double maxScore, double coeffDeltaAngle, double minSpanWidth) {
		superContours = superContours.stream().filter(contoursFilter).collect(Collectors.toList());
		Collections.sort(superContours);
		List<Edge> candidateEdges = new ArrayList<>();
		for (int i = 0; i < superContours.size(); i++)
			for (int j = 0; j < i; j++) {
				Edge edge = generateEdge(superContours.get(i), superContours.get(j), maxScore, coeffDeltaAngle);
				if (edge != null)
					candidateEdges.add(edge);
			}
		Collections.sort(candidateEdges);
		for (Edge edge : candidateEdges)
			if (edge.getC1().succ == null && edge.getC2().pred == null) {
				edge.getC1().succ = edge.getC2();
				edge.getC2().pred = edge.getC1();
			}
		List<SuperContoursSpan> spans = new ArrayList<>();
		while (!superContours.isEmpty()) {
			SuperContour contour = superContours.get(0);
			while (contour.pred != null)
				contour = contour.pred;
			SuperContoursSpan curSpan = new SuperContoursSpan();
			double width = 0.0;
			while (contour != null) {
				superContours.remove(contour);
				curSpan.add(contour);
				width += contour.lxmax - contour.lxmin;
				contour = contour.succ;
			}
			if (width > minSpanWidth && curSpan.getContours().size() > 1)
				spans.add(curSpan);
		}
		Collections.sort(spans);
		return spans;
	}

	private static Edge generateEdge(SuperContour c1, SuperContour c2, double maxScore, double coeffDeltaAngle) {

		if (c1.compareTo(c2) > 0) {
			SuperContour tmp = c1;
			c1 = c2;
			c2 = tmp;
		}
		double minDist = Double.MAX_VALUE;
		double c1Angle = 0;
		double c2Angle = 0;
		for (Point c1Point : new Point[] { c1.top, c1.right, c1.left, c1.bottom })
			for (Point c2Point : new Point[] { c2.top, c2.right, c2.left, c2.bottom }) {
				double dist = euclid(c1Point, c2Point);
				if (dist < minDist) {
					minDist = dist;
					c1Angle = toDemiPis((c1Point == c1.top || c1Point == c1.bottom) ? c1.antiAngle : c1.angle);
					c2Angle = toDemiPis((c2Point == c2.top || c2Point == c2.bottom) ? c2.antiAngle : c2.angle);
				}
			}

		double centersAngle = toDemiPis(Math.atan2(c2.center.y - c1.center.y, c2.center.x - c1.center.x));
		if ((Math.abs(centersAngle) * 180 / Math.PI) > 45)
			return null;
		double deltaAngle = Math.max(toZeroDemiPi(centersAngle - c1Angle), toZeroDemiPi(centersAngle - c2Angle));
		if (minDist * Math.sin(deltaAngle) > 8) {
			return null;
		}
		// System.out.println("not null " + c1Angle * 180 / Math.PI + " " + c2Angle * 180 / Math.PI + " " + centersAngle * 180 / Math.PI + " " + deltaAngle * 180 / Math.PI);
		double score = minDist + minDist * Math.sin(deltaAngle) * coeffDeltaAngle;
		// System.out.println("score " + score + " " + minDist + " " + minDist * Math.sin(deltaAngle) * coeffDeltaAngle + " " + deltaAngle * 180 / Math.PI);
		return score < maxScore ? new Edge(score, c1, c2) : null;
	}

	private static double toZeroDemiPi(double diff) {
		while (diff > Math.PI / 2)
			diff -= Math.PI;
		while (diff < -Math.PI / 2)
			diff += Math.PI;
		return Math.abs(diff);
	}

	private static double toDemiPis(double diff) {
		while (diff > Math.PI / 2)
			diff -= Math.PI;
		while (diff < -Math.PI / 2)
			diff += Math.PI;
		return diff;
	}

	private static double euclid(Point p1, Point p2) {
		return Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2));
	}
}