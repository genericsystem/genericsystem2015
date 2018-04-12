package org.genericsystem.cv.application;

import java.util.List;

import org.opencv.core.Point;

public class GeneralInterpolator implements Interpolator {

	private final List<OrientedPoint> horizontals;
	private final List<OrientedPoint> verticals;
	private final double pow;
	private final double minDist;

	public GeneralInterpolator(List<OrientedPoint> horizontals, List<OrientedPoint> verticals, double pow, double minDist) {
		this.horizontals = horizontals;
		this.verticals = verticals;
		this.pow = pow;
		this.minDist = minDist;
	}

	private double squaredEuclidianDistance(double x, double y, OrientedPoint op) { // distance euclidienne au carré
		double result = Math.pow(x - op.center.x, 2) + Math.pow(y - op.center.y, 2);
		return result >= minDist * minDist ? result : minDist * minDist;
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
			double hCoef = geoCoef * op.strenght;
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
			double vCoef = geoCoef * op.strenght;
			vAngle += vCoef * op.angle;
			sumVCoefs += vCoef;
		}
		return vAngle / sumVCoefs;
	}
}
