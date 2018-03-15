package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.application.SuperFrameImg.SuperContour;

public class SuperContourInterpolator {
	
	private final double pow;
	private List<SuperContour> superContours;
	
	private double distancesProduct;
	
	public SuperContourInterpolator(List<SuperContour> superContours, double pow) {
		this.superContours = superContours;
		this.pow = pow;
	}
	
	private double squaredEuclidianDistance(double x, double y, SuperContour sc) { // distance euclidienne au carré
		return Math.pow(x-sc.center.x, 2)+Math.pow(y-sc.center.y, 2); 
	}
	
	public double [] interpolate(double x, double y) { // retourne les angles horizontal et vertical interpolés
		
		double hCoef; // coefficient pour l'angle horizontal
		double vCoef; // coefficient pour l'angle vertical
		double sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
		double sumVCoefs = 0;
		double hAngle = 0; // angle horizontal
		double vAngle = 0; // angle vertical
		distancesProduct = 1;
		List<Double> distances = new ArrayList<>();
		superContours.forEach(sc -> {
			double distance = squaredEuclidianDistance(x, y, sc) / 100000; // sqrt(100000) = 316, on divise par un nombre de l'ordre de grandeur pour éviter de dépasser Double.MAX_VALUE si on a beaucoup de points
			distances.add(distance);
			distancesProduct *= distance;  
		});
		for(int p = 0 ; p < superContours.size() ; p++) {
			SuperContour sc = superContours.get(p);
			double geoCoef = Math.pow(distancesProduct / distances.get(p), pow);
			hCoef = geoCoef * sc.dx; // la largeur comme indice de confiance dans le coefficient
			hAngle += hCoef * sc.angle;
			vCoef = geoCoef;// * indice de confiance dy ?
			vAngle += vCoef * sc.antiAngle;
			sumHCoefs += hCoef;
			sumVCoefs += vCoef;
		};
		return new double[] { hAngle / sumHCoefs, vAngle / sumVCoefs };
	
	}

}