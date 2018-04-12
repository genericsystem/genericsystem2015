package org.genericsystem.cv.application;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GridInterpolator implements Interpolator {

	private static final Logger logger = LoggerFactory.getLogger(GridInterpolator.class);
	private int[] sampleSkewXs;
	private int[] sampleSkewYs;
	private double[][] skewDirs;
	private final List<SuperContour> superContours;
	private double hCoef = 0;
	private double hAngle = 0;
	private double sumHCoefs = 0;

	public GridInterpolator(List<SuperContour> superContours, List<Integer> patchXs, List<Integer> patchYs, int[][] dirs, int nSide, int nBin) {
		this.superContours = superContours;
		sampleSkewXs = new int[patchXs.size()];
		for (int i = 0; i < patchXs.size(); i++)
			sampleSkewXs[i] = patchXs.get(i) + nSide / 2;

		sampleSkewYs = new int[patchYs.size()];
		for (int i = 0; i < patchYs.size(); i++)
			sampleSkewYs[i] = patchYs.get(i) + nSide / 2;

		skewDirs = new double[dirs.length][dirs[0].length];
		for (int i = 0; i < skewDirs.length; i++)
			for (int j = 0; j < skewDirs[0].length; j++) {
				double angle = Math.PI * (dirs[i][j] - 1) / nBin;
				if (angle >= Math.PI)
					angle -= Math.PI;
				skewDirs[i][j] = angle;
			}
	}

	private double squaredEuclidianDistance(double x, double y, SuperContour sc) { // Squared euclidean distance
		double result = Math.pow(x - sc.center.x, 2) + Math.pow(y - sc.center.y, 2);
		double minDist = 30;
		return result >= Math.pow(minDist, 2) ? result : Math.pow(minDist, 2);
	}

	@Override
	public double interpolateHorizontals(double x, double y) {
		sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
		hAngle = 0; // angle horizontal
		superContours.forEach(sc -> {
			double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, sc) + 0.00001), 3.0 / 2); // on ajoute un epsilon pour éviter les divisions par 0
			hCoef = geoCoef * sc.dx; // la largeur comme indice de confiance dans le coefficient
			hAngle += hCoef * sc.angle;
			sumHCoefs += hCoef;
		});
		return hAngle / sumHCoefs;
	}

	@Override
	public double interpolateVerticals(double x, double y) {
		LambdaSearchResult xLambda = lambdaSearch(sampleSkewXs, x);
		LambdaSearchResult yLambda = lambdaSearch(sampleSkewYs, y);
		return (1 - xLambda.lambda) * ((1 - yLambda.lambda) * skewDirs[yLambda.indB][xLambda.indB] + yLambda.lambda * skewDirs[yLambda.indE][xLambda.indB])
				+ xLambda.lambda * ((1 - yLambda.lambda) * skewDirs[yLambda.indB][xLambda.indE] + yLambda.lambda * skewDirs[yLambda.indE][xLambda.indE]);
	}

	static class LambdaSearchResult {
		final int indB;
		final int indE;
		final double lambda;

		public LambdaSearchResult(int indB, int indE, double lambda) {
			this.indB = indB;
			this.indE = indE;
			this.lambda = lambda;
		}

		@Override
		public String toString() {
			return "{ indB: " + indB + ", indE: " + indE + ", lambda: " + lambda + " }";
		}
	}

	private LambdaSearchResult lambdaSearch(int[] xs, double x) {
		int indB = xs.length - 1;
		int indE = 0;
		double lambda = 0;
		while (indB >= 0 && x < xs[indB])
			indB--;
		if (indB == xs.length - 1) { // x >= xs[xs.length - 1]
			indE = indB;
		} else if (indB == -1) { // x < xs[0]
			indB++;
		} else { // General case.
			indE = indB + 1;
			lambda = (x - xs[indB]) / (xs[indE] - xs[indB]);
		}
		return new LambdaSearchResult(indB, indE, lambda);
	}
}
