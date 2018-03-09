package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class WeightedOrientedPointsInterpolator {
	
	private final double pow;
	private final double [] [] weightedOrientedPoints;
	
	public WeightedOrientedPointsInterpolator(double [] [] weightedOrientedPoints, double pow) {
		this.weightedOrientedPoints = weightedOrientedPoints;
		this.pow = pow;
	}
	
	private double squaredEuclidianDistance(double x, double y, double[] orientedPoint) { // distance euclidienne au carré
		return Math.pow(x-orientedPoint[0], 2)+Math.pow(y-orientedPoint[1], 2); 
	}
	
	private double coefficient(List<Double> distances, int n, double pow){
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
	
	public double interpolate(double x, double y) {
		
		List<Double> distances = Arrays.stream(weightedOrientedPoints).map(p -> squaredEuclidianDistance(x, y, p)).collect(Collectors.toList());
		double coef;
		double sumCoefs = 0;
		double theta = 0;
		for(int p = 0 ; p < weightedOrientedPoints.length ; p++) {
			coef = coefficient(distances, p, pow) * weightedOrientedPoints[p][3]; // multiplication par l'indice de confiance
			theta += coef * weightedOrientedPoints[p][2];
			sumCoefs += coef;
		};
		return theta / sumCoefs;
	
	}
		
}