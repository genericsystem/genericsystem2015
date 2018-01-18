package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class Lines {

	final List<Line> lines;

	public Lines(Mat src) {
		lines = new ArrayList<>();
		for (int i = 0; i < src.rows(); i++) {
			double[] val = src.get(i, 0);
			Line line = new Line(val[0], val[1], val[2], val[3]);
			lines.add(line);
		}
	}

	public Lines filter(Predicate<Line> predicate) {
		return new Lines(lines.stream().filter(predicate).collect(Collectors.toList()));
	}

	public Lines reduce(int max) {
		if (lines.size() <= max)
			return this;

		Set<Line> newLines = new HashSet<>();
		while (newLines.size() < max)
			newLines.add(lines.get((int) (Math.random() * size())));
		return new Lines((newLines));
	}

	public Lines(Collection<Line> lines) {
		this.lines = new ArrayList<>(lines);
	}

	public Lines rotate(Mat matrix) {
		return new Lines(lines.stream().map(line -> line.transform(matrix)).collect(Collectors.toList()));
	}

	public Lines perspectivTransform(Mat matrix) {
		return new Lines(lines.stream().map(line -> line.perspectivTransform(matrix)).collect(Collectors.toList()));
	}

	public void draw(Mat frame, Scalar color, int thickness) {
		lines.forEach(line -> line.draw(frame, color, thickness));
	}

	public int size() {
		return lines.size();
	}


	public static class Line {

		final double x1, y1, x2, y2;

		public Line(Point p1, Point p2) {
			this(p1.x, p1.y, p2.x, p2.y);
		}

		public Line(double x1, double y1, double x2, double y2) {
			this.x1 = x1;
			this.x2 = x2;
			this.y1 = y1;
			this.y2 = y2;
		}

		public double size() {
			return Math.sqrt(Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2));
		}

		public Line transform(Mat rotationMatrix) {
			MatOfPoint2f results = new MatOfPoint2f();
			Core.transform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2))), results, rotationMatrix);
			Point[] targets = results.toArray();
			return new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		}

		public Line perspectivTransform(Mat homography) {
			MatOfPoint2f results = new MatOfPoint2f();
			Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2))), results, homography);
			Point[] targets = results.toArray();
			return new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		}

		public void draw(Mat frame, Scalar color, int thickness) {
			Imgproc.line(frame, new Point(x1, y1), new Point(x2, y2), color, thickness);
		}
	}
}
