package org.genericsystem.cv;

import java.util.function.Function;

public class Recuit {

	public static void main(String[] args) {
		System.out.println(phi(-2.048));
	}

	static double phi(double x) {
		double s = x, t = 0, b = x, q = x * x;
		int i = 1;

		while (s != t)
			s = (t = s) + (b *= q / (i += 2));
		return 0.5 + s * Math.exp(-0.5 * q - 0.91893853320467274178d);
	}

	// public static void main(String[] args) {
	// Function<Double, Double> function0 = x -> (2 + 1.5 * Math.cos((x - 3) * Math.PI)) + 0.3 * Math.pow(1 - x + 3, 2);
	// Function<Double, Double> function1 = x -> (2 + 1.5 * Math.cos((x) * Math.PI)) + 0.3 * Math.pow(1 - x, 2);
	// Function<Double, Double> function2 = x -> (2 + 1.5 * Math.cos((x - 5) * Math.PI)) + 0.3 * Math.pow(1 - x + 5, 2);
	// System.out.println(Arrays.toString(round(min(distance(function0, function1, function2), new Double[] { 10., 10., 10. }, T -> T * 0.99, 1000))));
	// }

	private static Function<Double[], Double> distance(Function<Double, Double>... functions) {
		return x -> {
			double result = 0;
			for (int i = 0; i < x.length; i++)
				result += Math.pow(functions[i].apply(x[i]), 2);
			return Math.sqrt(result);
		};
	}

	static Double[] min(Function<Double[], Double> f, Double[] T0, Function<Double, Double> decrease, int count) {
		Double[] bestx = T0;
		for (double T = 1; T > 0.01; T = decrease.apply(T)) {
			Double[] bestxforT = minForT(f, T, bestx, count);
			if (f.apply(bestxforT) < f.apply(bestx))
				bestx = bestxforT;
		}
		System.out.println("f(x) min : " + f.apply(bestx));
		return bestx;
	}

	static Double[] minForT(Function<Double[], Double> f, double T, Double[] bestx, int count) {
		Double[] superBestx = bestx;
		for (int i = 0; i < count; i++) {
			Double[] x = generateXForT(bestx, T);
			double delta = f.apply(x) - f.apply(bestx);
			double delta2 = f.apply(x) - f.apply(superBestx);
			if (delta2 < 0)
				superBestx = x;
			if (delta < 0)
				bestx = x;
			else {
				if (Math.random() < Math.exp(-delta / T))
					bestx = x;
			}
		}
		return superBestx;
	}

	static Double[] generateXForT(Double[] x, double T) {
		Double[] result = new Double[x.length];
		for (int i = 0; i < x.length; i++)
			result[i] = x[i] + (Math.random() - Math.random()) * (T) * x[i];
		return result;
	}

	static Double[] round(Double[] x) {
		Double[] result = new Double[x.length];
		for (int i = 0; i < x.length; i++)
			result[i] = Double.valueOf(Math.round(x[i] * 1000)) / 1000;
		return result;
	}

}
