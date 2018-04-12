package org.genericsystem.cv.application;

import java.util.List;

import org.opencv.core.Point;

public class GeneralInterpolator implements Interpolator {

	private final double pow;
	private List<OrientedPoint> horizontals;
	private List<OrientedPoint> verticals;

	private double hCoef; // coefficient pour l'angle horizontal
	private double vCoef; // coefficient pour l'angle vertical

	public GeneralInterpolator(List<OrientedPoint> horizontals, List<OrientedPoint> verticals, double pow) {
		this.horizontals = horizontals;
		this.verticals = verticals;
		this.pow = pow;
	}

	private double squaredEuclidianDistance(double x, double y, OrientedPoint op) { // distance euclidienne au carré
		double result = Math.pow(x - op.center.x, 2) + Math.pow(y - op.center.y, 2);
		double minDist = 100;
		return result >= Math.pow(minDist, 2) ? result : Math.pow(minDist, 2);
	}

	public static class OrientedPoint {

		private Point center;
		private double strenght;
		private double angle;

		public OrientedPoint(Point center, double angle, double strenght) {
			this.center = center;
			this.strenght = strenght;
			this.angle = angle;
		}
	}

	@Override
	public double interpolateHorizontals(double x, double y) {
		double sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
		double hAngle = 0; // angle horizontal
		for (OrientedPoint op : horizontals) {
			double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, op)), pow / 2);
			hCoef = geoCoef * op.strenght;
			hAngle += hCoef * op.angle;
			sumHCoefs += hCoef;
		}
		return hAngle / sumHCoefs;
	}

	@Override
	public double interpolateVerticals(double x, double y) {
		double vAngle = 0;
		double sumVCoefs = 0;
		for (OrientedPoint op : verticals) {
			double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, op)), pow / 2);
			vCoef = geoCoef * op.strenght;
			vAngle += vCoef * op.angle;
			sumVCoefs += vCoef;
		}
		return vAngle / sumVCoefs;
	}
}
