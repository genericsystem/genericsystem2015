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
		// List<Circle> circles = detectCircles(superFrame.getDiffFrame(), 50, 30, 100);
		List<Circle> circles = gridCircles(superFrame.size(), (int) (superFrame.size().width / 15));
		Collection<Circle> selectedCircles = selectRandomObjects(circles, 30);
		List<Line> result = new ArrayList<>();
		for (Circle circle : selectedCircles) {
			Img circledImg = getCircledImg(superFrame, (int) circle.radius, circle.center);
			double angle = getBestAngle(circledImg, 42, 12, 5, 192, null) / 180 * Math.PI;
			result.add(buildLine(circle.center, angle, circle.radius));
			Imgproc.circle(superFrame.getDisplay().getSrc(), circle.center, (int) circle.radius, new Scalar(0, 255, 0), 1);
		}

		// List<Rect> rects = detectRects(superFrame.getDiffFrame(), 50, 60, 200, 10, 50);
		// Collection<Rect> selectedRects = selectRandomObjects(rects, 30);
		// List<Line> result2 = new ArrayList<>();
		// for (Rect rect : selectedRects) {
		// Img rectImg = getRectImg(superFrame, rect);
		// double angle = getBestAngle(rectImg, 42, 12, 5, 192, null) / 180 * Math.PI;
		// result2.add(buildLine(new Point(rect.tl().x + rect.width / 2, rect.tl().y + rect.height / 2), angle, rect.width / 2));
		// Imgproc.rectangle(superFrame.getDisplay().getSrc(), rect.tl(), rect.br(), new Scalar(0, 0, 255), 1);
		// }
		// return result2;
		return result;
	}

	private static List<Circle> gridCircles(Size size, double radius) {
		List<Circle> circles = new ArrayList<>();
		for (int j = 2; j <= (size.width / radius - 2); j += 2)
			for (int i = 2; i <= (size.height / radius - 2); i += 2) {
				circles.add(new Circle(new Point(j * radius, i * radius), (int) radius));
			}
		return circles;
	}

	private static List<MatOfPoint> getContours(Img img) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(img.getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		return contours;
	}

	private static List<Circle> detectCircles(Img img, int maxContourArea, int minRadius, int maxRadius) {
		List<MatOfPoint> contours = getContours(img);
		List<Circle> circles = new ArrayList<>();
		for (MatOfPoint contour : contours) {
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > 50) {
				float[] radius = new float[1];
				Point center = new Point();
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				Imgproc.minEnclosingCircle(contour2F, center, radius);
				if (radius[0] > minRadius && radius[0] < maxRadius && center.x > radius[0] && center.y > radius[0] && ((center.x + radius[0]) < img.width()) && ((center.y + radius[0]) < img.height()))
					circles.add(new Circle(center, radius[0]));
			}
		}
		return circles;
	}

	private static List<Rect> detectRects(Img img, int maxContourArea, int minWidth, int maxWidth, int minHeight, int maxHeight) {
		List<MatOfPoint> contours = getContours(img);
		List<Rect> rects = new ArrayList<>();
		for (MatOfPoint contour : contours) {
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > 50) {
				Rect rect = Imgproc.boundingRect(contour);
				if (rect.width >= minWidth && rect.width <= maxWidth && rect.height >= minHeight && rect.height <= maxHeight)
					rects.add(rect);
			}
		}
		return rects;
	}

	private static <T> Collection<T> selectRandomObjects(List<T> objects, int maxReturnObjects) {
		if (objects.size() <= maxReturnObjects)
			return objects;
		Set<T> result = new HashSet<>();
		while (result.size() < maxReturnObjects)
			result.add(objects.get((int) (Math.random() * objects.size())));
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

	private static Img getRectImg(SuperFrameImg superFrame, Rect rect) {
		return new Img(new Mat(superFrame.getBinarized().getSrc(), rect), false);
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
		Point center;
		float radius;

		public Circle(Point center, float radius) {
			this.center = center;
			this.radius = radius;
		}
	}
}
