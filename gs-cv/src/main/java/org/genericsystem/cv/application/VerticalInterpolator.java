package org.genericsystem.cv.application;

import java.util.List;


public class VerticalInterpolator {

	private int[] sampleSkewXs;
	private int[] sampleSkewYs;
	private double[][] skewDirs;

	public VerticalInterpolator(List<Integer> patchXs, List<Integer> patchYs, int[][] dirs, int nSide, int nBin) {
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

	// Change only the computation of the vertical angles from SuperContourInterpolator.
	public double interpolate(double x, double y) {
		LambdaSearchResult xLambda = lambdaSearch(sampleSkewXs, x);
		LambdaSearchResult yLambda = lambdaSearch(sampleSkewYs, y);
		double vAngle = (1 - xLambda.lambda) * ((1 - yLambda.lambda) * skewDirs[yLambda.indB][xLambda.indB] + yLambda.lambda * skewDirs[yLambda.indE][xLambda.indB])
				+ xLambda.lambda * ((1 - yLambda.lambda) * skewDirs[yLambda.indB][xLambda.indE] + yLambda.lambda * skewDirs[yLambda.indE][xLambda.indE]);
		return vAngle;
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
