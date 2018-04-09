package org.genericsystem.cv.application;

import java.util.List;

import org.opencv.core.Point;

public class GeneralInterpolator implements Interpolator {

	private final double pow;
	private List<OrientedPoint> horizontals;
	private List<OrientedPoint> verticals;

	private double hCoef; // coefficient pour l'angle horizontal
	private double vCoef; // coefficient pour l'angle vertical
	private double sumHCoefs; // somme des coefficients pour l'angle horizontal
	private double sumVCoefs;
	private double hAngle; // angle horizontal
	private double vAngle; // angle vertical

	public GeneralInterpolator(List<OrientedPoint> horizontals, List<OrientedPoint> verticals, double pow) {
		this.horizontals = horizontals;
		this.verticals = verticals;
		this.pow = pow;
	}

	private double squaredEuclidianDistance(double x, double y, OrientedPoint op) { // distance euclidienne au carré
		double result = Math.pow(x - op.center.x, 2) + Math.pow(y - op.center.y, 2);
		double minDist = 0;
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
	public double[] interpolate(double x, double y) { // retourne les angles horizontal et vertical interpolés
		sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
		sumVCoefs = 0;
		hAngle = 0; // angle horizontal
		vAngle = 0;
		horizontals.forEach(h -> {
			double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, h) + 0.00001), pow / 2); // on ajoute un epsilon pour éviter les divisions par 0
			hCoef = geoCoef * h.strenght; // la largeur comme indice de confiance dans le coefficient
			hAngle += hCoef * h.angle;
			sumHCoefs += hCoef;
		});
		verticals.forEach(v -> {
			double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, v) + 0.00001), pow / 2); // on ajoute un epsilon pour éviter les divisions par 0
			vCoef = geoCoef * v.strenght;// * indice de confiance dy ?
			vAngle += vCoef * v.angle;
			sumHCoefs += hCoef;
			sumVCoefs += vCoef;
		});

		return new double[] { hAngle / sumHCoefs, vAngle / sumVCoefs };

	}

}
