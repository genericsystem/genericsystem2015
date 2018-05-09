package org.genericsystem.cv.application;

class TrajectStep implements Comparable<TrajectStep> {
	public final int k;
	public int theta;
	public final double magnitude;

	public TrajectStep(int k, int theta, double magnitude) {
		this.k = k;
		this.theta = theta;
		this.magnitude = magnitude;
	}

	@Override
	public int compareTo(TrajectStep step) {
		return Double.compare(step.magnitude, magnitude);
	}

}