package org.genericsystem.cv;

import java.util.List;

import org.opencv.core.Mat;
import org.opencv.core.Point;

public class Reconciliation {

	private final Mat homography;
	private final List<Point> newPts;
	private final List<Point> referencePts;

	public Reconciliation(Mat homography, List<Point> newPts, List<Point> referencePts) {
		this.homography = homography;
		this.newPts = newPts;
		this.referencePts = referencePts;
	}

	public Mat getHomography() {
		return homography;
	}

	public List<Point> getPts() {
		return newPts;
	}

	public List<Point> getReferencePts() {
		return referencePts;
	}

}
