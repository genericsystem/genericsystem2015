package org.genericsystem.cv.application;

import java.util.List;

import org.genericsystem.cv.application.SuperFrameImg.SuperContour;

public class SuperContourInterpolator {

	private final double pow;
	private List<SuperContour> superContours;

	private double hCoef; // coefficient pour l'angle horizontal
	private double vCoef; // coefficient pour l'angle vertical
	private double sumHCoefs; // somme des coefficients pour l'angle horizontal
	private double sumVCoefs;
	private double hAngle; // angle horizontal
	private double vAngle; // angle vertical

	public SuperContourInterpolator(List<SuperContour> superContours, double pow) {
		this.superContours = superContours;
		this.pow = pow;
	}

	private double squaredEuclidianDistance(double x, double y, SuperContour sc) { // distance euclidienne au carré
		double result = Math.pow(x - sc.center.x, 2) + Math.pow(y - sc.center.y, 2);

		return result >= 400 ? result : 400;
	}

	public double[] interpolate(double x, double y) { // retourne les angles horizontal et vertical interpolés

		sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
		sumVCoefs = 0;
		hAngle = 0; // angle horizontal
		vAngle = 0;
		superContours.forEach(sc -> {
			double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, sc) + 0.00001), pow / 2); // on ajoute un epsilon pour éviter les divisions par 0
			hCoef = geoCoef * sc.dx; // la largeur comme indice de confiance dans le coefficient
			hAngle += hCoef * sc.angle;
			vCoef = geoCoef * sc.dx;// * indice de confiance dy ?
			vAngle += vCoef * sc.vertical;
			sumHCoefs += hCoef;
			sumVCoefs += vCoef;
		});
		return new double[] { hAngle / sumHCoefs, vAngle / sumVCoefs };

	}

}