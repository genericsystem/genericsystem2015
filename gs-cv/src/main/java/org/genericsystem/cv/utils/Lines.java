package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;

public class Lines implements Iterable<Line> {
	protected final List<Line> lines = new ArrayList<>();
	protected final double mean;

	public Lines(Mat src) {
		double mean = 0;
		for (int i = 0; i < src.rows(); i++) {
			double[] val = src.get(i, 0);
			Line line = new Line(val[0], val[1], val[2], val[3]);
			lines.add(line);
			mean += line.getAngle();
		}
		this.mean = mean / src.rows();
	}

	public Lines(Collection<Line> lines) {
		double mean = 0;
		for (Line line : lines) {
			this.lines.add(line);
			mean += line.getAngle();
		}
		this.mean = mean / lines.size();
	}

	public List<Point[]> toLinesSegments() {
		return lines.stream().map(line -> line.getSegment()).collect(Collectors.toList());
	}

	public List<Line> rotate(Mat matrix) {
		return lines.stream().map(line -> line.rotationTransform(matrix)).collect(Collectors.toList());
	}

	public List<Line> perspectivTransform(Mat matrix) {
		return lines.stream().map(line -> line.perspectivTransform(matrix)).collect(Collectors.toList());
	}

	public void draw(Mat frame, Scalar color) {
		lines.forEach(line -> line.draw(frame, color));
	}

	public int size() {
		return lines.size();
	}

	public double getMean() {
		return mean;
	}

	@Override
	public Iterator<Line> iterator() {
		return lines.iterator();
	}
}
