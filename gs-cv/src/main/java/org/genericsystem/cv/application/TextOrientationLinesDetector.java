package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Lines.Line;
import org.genericsystem.cv.lm.LevenbergImpl;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class TextOrientationLinesDetector {

	static List<Line> getTextOrientationLines(SuperFrameImg superFrame) {
		List<Circle> circles = detectCircles(superFrame, 30, 100);
		Collection<Circle> selectedCircles = selectRandomCirles(circles, 20);
		List<Line> result = new ArrayList<>();
		for (Circle circle : selectedCircles) {
			Img circledImg = getCircledImg(superFrame, (int) circle.radius, circle.center);
			double angle = getBestAngle(circledImg, 42, 12, 5, 192, null) / 180 * Math.PI;
			result.add(buildLine(circle.center, angle, circle.radius));
			Imgproc.circle(superFrame.getDisplay().getSrc(), circle.center, (int) circle.radius, new Scalar(0, 255, 0), 1);
		}
		return result;
	}

	private static List<Circle> detectCircles(SuperFrameImg superFrame, int minRadius, int maxRadius) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(superFrame.getDiffFrame().getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		List<Circle> circles = new ArrayList<>();
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > 50) {
				float[] radius = new float[1];
				Point center = new Point();
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				Imgproc.minEnclosingCircle(contour2F, center, radius);
				if (radius[0] > minRadius && radius[0] < maxRadius && center.x > radius[0] && center.y > radius[0] && ((center.x + radius[0]) < superFrame.width()) && ((center.y + radius[0]) < superFrame.height())) {
					circles.add(new Circle(center, radius[0]));
					// Imgproc.circle(frame, center, (int) radius[0], new Scalar(0, 0, 255));
				}
				// Imgproc.drawContours(superFrame.getDisplay().getSrc(), Arrays.asList(contour), 0, new Scalar(0, 0, 255), 1);
			}
		}
		return circles;
	}

	private static Collection<Circle> selectRandomCirles(List<Circle> circles, int circlesNumber) {
		if (circles.size() <= circlesNumber)
			return circles;
		Set<Circle> result = new HashSet<>();
		while (result.size() < circlesNumber)
			result.add(circles.get((int) (Math.random() * circles.size())));
		return result;
	}

	private static Img getCircledImg(SuperFrameImg superFrame, int radius, Point center) {
		Mat mask = new Mat(new Size(radius * 2, radius * 2), CvType.CV_8UC1, new Scalar(0));
		Imgproc.circle(mask, new Point(radius, radius), radius, new Scalar(255), -1);
		Rect rect = new Rect(new Point(center.x - radius, center.y - radius), new Point(center.x + radius, center.y + radius));
		Mat roi = new Mat(superFrame.getBinarized().getSrc(), rect);
		Mat circled = new Mat();
		roi.copyTo(circled, mask);
		Img circledImg = new Img(circled, false);
		return circledImg;
	}

	private static Line buildLine(Point center, double angle, double size) {
		double x1 = center.x - Math.sin(angle) * size;
		double y1 = center.y + Math.cos(angle) * size;
		double x2 = center.x + Math.sin(angle) * size;
		double y2 = center.y - Math.cos(angle) * size;
		return new Line(new Point(x1, y1), new Point(x2, y2));
	}

	private static double score(Img circled, double angle, int filterSize, double threshold) {
		Mat M = Imgproc.getRotationMatrix2D(new Point(circled.width() / 2, circled.width() / 2), angle, 1);
		Mat rotated = new Mat();
		Imgproc.warpAffine(circled.getSrc(), rotated, M, new Size(circled.width(), circled.width()));
		Img binarized = new Img(rotated, false).directionalFilter(filterSize).thresHold(threshold, 255, Imgproc.THRESH_BINARY);
		Mat result = new Mat();
		Core.reduce(binarized.getSrc(), result, 1, Core.REDUCE_SUM, CvType.CV_64F);
		Core.reduce(result, result, 0, Core.REDUCE_SUM, CvType.CV_64F);
		return result.get(0, 0)[0];
	}

	private static double getBestAngle(Img circledImg, int absMinMax, double step, int filterSize, double threshold, Img[] binarized) {
		double maxScore = 0;
		double bestAngle = -1;
		if (binarized != null)
			binarized[0] = new Img(new Mat(new Size(2 * absMinMax * 10, 200), CvType.CV_8UC1, new Scalar(0)), false);
		List<double[]> results = new ArrayList<>();
		for (double angle = -absMinMax; angle <= absMinMax; angle += step) {
			double score = score(circledImg, angle, filterSize, threshold);
			if (angle != 0 && score > maxScore) {
				maxScore = score;
				bestAngle = angle;
			}
			if (angle != 0)
				results.add(new double[] { angle, score });
			// System.out.println(score);
			if (binarized != null)
				new Line((absMinMax + angle) * 10, 0, (absMinMax + angle) * 10, score / 1000).draw(binarized[0].getSrc(), new Scalar(255, 0, 0), 1);
		}
		BiFunction<Double, double[], Double> f = (x, params) -> params[0] * x * x * x * x + params[1] * x * x * x + params[2] * x * x + params[3] * x + params[4];
		BiFunction<double[], double[], Double> e = (xy, params) -> f.apply(xy[0], params) - xy[1];
		double[] result = new LevenbergImpl<>(e, results, new double[] { 1, 1, 1, 1, 1 }).getParams();
		Point point = null;
		double polynomAngle = 0.0;
		double max = 0.0;
		for (double angle = -absMinMax; angle <= absMinMax; angle++) {
			Point oldPoint = point;
			double score = f.apply(angle, result);
			point = new Point((absMinMax + angle) * 10, score / 1000);
			if (score > max) {
				max = score;
				polynomAngle = angle;
			}
			if (binarized != null && oldPoint != null)
				new Line(oldPoint, point).draw(binarized[0].getSrc(), new Scalar(255, 0, 0), 1);
		}
		if (binarized != null) {
			Imgproc.circle(binarized[0].getSrc(), new Point((absMinMax + polynomAngle) * 10, max / 1000), 10, new Scalar(255, 255, 0), 3);
			// new Line(new Point((absMinMax + bestAngle) * 10, maxScore / 1000), new Point((absMinMax + bestAngle) * 10, 0)).draw(binarized[0].getSrc(), new Scalar(255, 255, 0), 3);
		}
		// System.out.println(Arrays.toString(result));

		return polynomAngle;
	}

	static class Circle {
		public Circle(Point center, float radius) {
			this.center = center;
			this.radius = radius;
		}

		Point center;
		float radius;
	}
}
