package org.genericsystem.cv.application;

public class HoughTrajectStep implements Comparable<HoughTrajectStep> {
	final int y;
	double derivative;
	final double magnitude;

	public HoughTrajectStep(int y, double derivative, double magnitude) {
		this.y = y;
		this.derivative = derivative;
		this.magnitude = magnitude;
	}

	@Override
	public int compareTo(HoughTrajectStep step) {
		return Double.compare(step.magnitude, magnitude);
	}

	public double getTheta() {
		return Math.atan(derivative) / Math.PI * 180 + 45;
	}

}