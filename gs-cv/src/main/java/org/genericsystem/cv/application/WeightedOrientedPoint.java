package org.genericsystem.cv.application;

public class WeightedOrientedPoint {
	
	public double x, y;
	public double hAngle; // angle qui doit être compris entre -pi/2 et pi/2
	public double hAngleConfidence;
	public double vAngle; // angle qui doit être compris entre 0 et pi
	public double vAngleConfidence;
	
	public WeightedOrientedPoint(double x, double y, double hAngle, double hAngleConfidence, double vAngle,	double vAngleConfidence) {
		this.x = x;
		this.y = y;
		this.hAngle = hAngle;
		this.hAngleConfidence = hAngleConfidence;
		this.vAngle = vAngle;
		this.vAngleConfidence = vAngleConfidence;
	}

}
