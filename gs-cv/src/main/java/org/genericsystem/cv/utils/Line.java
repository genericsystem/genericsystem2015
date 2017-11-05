package org.genericsystem.cv.utils;

import java.util.Arrays;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class Line {
	public final double x1, y1, x2, y2;
	protected final double angle;

	public Line(Point p1, Point p2) {
		this(p1.x, p1.y, p2.x, p2.y);
	}

	public Line(double x1, double y1, double x2, double y2) {
		this.x1 = x1;
		this.x2 = x2;
		this.y1 = y1;
		this.y2 = y2;
		this.angle = Math.atan2(y2 - y1, x2 - x1);
	}

	public Point[] getSegment() {
		return new Point[] { new Point(x1, y1), new Point(x2, y2) };
	}

	public Line rotationTransform(Mat rotationMatrix) {
		Mat original = Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2)));
		MatOfPoint2f results = new MatOfPoint2f();
		Core.transform(original, results, rotationMatrix);
		Point[] targets = results.toArray();
		Line line = new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		original.release();
		results.release();
		return line;
	}

	public Line perspectivTransform(Mat homography) {
		Mat original = Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2)));
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(original, results, homography);
		Point[] targets = results.toArray();
		Line line = new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		original.release();
		results.release();
		return line;
	}

	public void draw(Mat frame, Scalar color, int thickness) {
		Imgproc.line(frame, new Point(x1, y1), new Point(x2, y2), color, thickness);
	}

	public double size() {
		return Math.sqrt(Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2));
	}

	public double geta() {
		return (y2 - y1) / (x2 - x1);
	}

	public double getb() {
		return y1 - geta() * x1;
	}

	public Point intersection(Line line) {
		double x = (line.getb() - getb()) / (geta() - line.geta());
		double y = geta() * x + getb();
		return new Point(x, y);
	}

	public Point intersection(double verticalLinex) {
		double x = verticalLinex;
		double y = geta() * x + getb();
		return new Point(x, y);
	}

	@Override
	public String toString() {
		return "Line : " + angle;
	}

	public double getAngle() {
		return angle;
	}

	public double getX1() {
		return x1;
	}

	public double getY1() {
		return y1;
	}

	public double getX2() {
		return x2;
	}

	public double getY2() {
		return y2;
	}
}
