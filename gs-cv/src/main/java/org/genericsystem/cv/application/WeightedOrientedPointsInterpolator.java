package org.genericsystem.cv.application;

import java.util.List;
import java.util.stream.Collectors;

public class WeightedOrientedPointsInterpolator {
	
	private final double pow;
	private List<WeightedOrientedPoint> weightedOrientedPoints;
	
	public WeightedOrientedPointsInterpolator(List<WeightedOrientedPoint> weightedOrientedPoints, double pow) {
		this.weightedOrientedPoints = weightedOrientedPoints;
		this.pow = pow;
	}
	
	private double squaredEuclidianDistance(double x, double y, WeightedOrientedPoint p) { // distance euclidienne au carré
		return Math.pow(x-p.x, 2)+Math.pow(y-p.y, 2); 
	}
	
	private double geoCoef(List<Double> distances, int n, double pow){ // coefficient lié à la distance
		double coef = 1;
		int k = 0;
		for(double distance : distances) {
			if(k != n) {
				coef = coef  * Math.pow(distance/1000000, pow); // On divise d^2 par 1000000 pour s'assurer qu'on ne va pas dépasser la valeur max du double
			}
			k++;
		}
		return coef;
	}
	
	public double [] interpolate(double x, double y) { // retourne les angles horizontal et vertical interpolés
		
		List<Double> distances = weightedOrientedPoints.stream().map(p -> squaredEuclidianDistance(x, y, p)).collect(Collectors.toList());
		double hCoef; // coefficient pour l'angle horizontal
		double vCoef; // coefficient pour l'angle vertical
		double sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
		double sumVCoefs = 0;
		double hAngle = 0; // angle horizontal
		double vAngle = 0; // angle vertical
		for(int p = 0 ; p < weightedOrientedPoints.size() ; p++) {
			double geoCoef = geoCoef(distances, p, pow);
			hCoef = geoCoef * weightedOrientedPoints.get(p).hAngleConfidence; // multiplication par l'indice de confiance
			hAngle += hCoef * weightedOrientedPoints.get(p).hAngle;
			vCoef = geoCoef * weightedOrientedPoints.get(p).vAngleConfidence;
			vAngle += vCoef * weightedOrientedPoints.get(p).vAngle;
			sumHCoefs += hCoef;
			sumVCoefs += vCoef;
		};
		return new double[] { hAngle / sumHCoefs, vAngle / sumVCoefs };
	
	}
		
}