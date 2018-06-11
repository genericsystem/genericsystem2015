package org.genericsystem.cv.application;

public class TrajectStep implements Comparable<TrajectStep> {
	final int y;
	double derivative;
	final double magnitude;

	public TrajectStep(int y, double derivative, double magnitude) {
		this.y = y;
		this.derivative = derivative;
		this.magnitude = magnitude;
	}

	@Override
	public int compareTo(TrajectStep step) {
		return Double.compare(step.magnitude, magnitude);
	}

}