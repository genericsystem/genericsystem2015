package org.genericsystem.cv.application;

import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class OrientedPoint {

	Point center;
	double strenght;
	double angle;
	double derivative;

	public OrientedPoint(Point center, double angle, double strenght, double derivate) {
		this.center = center;
		this.strenght = strenght;
		this.angle = angle;
		this.derivative = derivate;
	}

	public void displayHorizontalOp(Mat img, double vStep, double hStep, Scalar color) {
		Imgproc.line(img, new Point(center.x - Math.cos(angle) * vStep * strenght, center.y - Math.sin(angle) * hStep * strenght), new Point(center.x + Math.cos(angle) * vStep * strenght, center.y + Math.sin(angle) * hStep * strenght), color, 1);
	}

	public void displayVerticalOp(Mat img, double vStep, double hStep, Scalar color) {
		Imgproc.line(img, new Point(center.x + Math.sin(angle) * vStep * strenght, center.y - Math.cos(angle) * hStep * strenght), new Point(center.x - Math.sin(angle) * vStep * strenght, center.y + Math.cos(angle) * hStep * strenght), color, 1);
	}
}