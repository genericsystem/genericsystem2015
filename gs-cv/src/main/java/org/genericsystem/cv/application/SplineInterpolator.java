package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;

public class SplineInterpolator implements Interpolator {

	private final List<PolynomialSplineFunction[]> horizontals;
	private final List<PolynomialSplineFunction[]> verticals;

	private final Interpolator subInterpolator;

	public SplineInterpolator(Interpolator subInterpolator, List<PolynomialSplineFunction>[] horizontals, List<PolynomialSplineFunction>[] verticals) {
		this.horizontals = Arrays.stream(horizontals).flatMap(h -> h.stream()).map(f -> new PolynomialSplineFunction[] { f, f.polynomialSplineDerivative() }).collect(Collectors.toList());
		this.verticals = Arrays.stream(verticals).flatMap(h -> h.stream()).map(f -> new PolynomialSplineFunction[] { f, f.polynomialSplineDerivative() }).collect(Collectors.toList());

		this.subInterpolator = subInterpolator;
	}

	@Override
	public double interpolateVerticals(double x, double y) {
		PolynomialSplineFunction[] left = findLeftVerticalSplines(x, y);
		PolynomialSplineFunction[] right = findRightVerticalSplines(x, y);
		if (left != null && right != null) {
			double x1 = left[0].value(y);
			double x2 = right[0].value(y);
			return -((x - x1) / (x2 - x1) * Math.atan(right[1].value(y)) + (x2 - x) / (x2 - x1) * Math.atan(left[1].value(y)));
		} else
			return subInterpolator.interpolateVerticals(x, y);
		// if (left != null) {
		// if (right != null) {
		// double x1 = left[0].value(y);
		// double x2 = right[0].value(y);
		// return (x - x1) * left[1].value(y) / (x2 - x1) + (x2 - x) * right[1].value(y) / (x2 - x1);
		// } else
		// return left[1].value(y);
		// } else {
		// if (right != null)
		// return right[1].value(y);
		// else
		// return subInterpolator.interpolateVerticals(x, y);
		// }

	}

	private PolynomialSplineFunction[] findRightVerticalSplines(double x, double y) {
		double value = Double.POSITIVE_INFINITY;
		PolynomialSplineFunction[] result = null;
		for (PolynomialSplineFunction[] function : verticals) {
			if (function[0].isValidPoint(y)) {
				double polyValue = function[0].value(y);
				if (polyValue < value && polyValue >= x) {
					value = polyValue;
					result = function;
				}
			}
		}
		return result;
	}

	private PolynomialSplineFunction[] findLeftVerticalSplines(double x, double y) {
		double value = Double.NEGATIVE_INFINITY;
		PolynomialSplineFunction[] result = null;
		for (PolynomialSplineFunction[] function : verticals) {
			if (function[0].isValidPoint(y)) {
				double polyValue = function[0].value(y);
				if (polyValue > value && polyValue <= x) {
					value = polyValue;
					result = function;
				}
			}
		}
		return result;
	}

	@Override
	public double interpolateHorizontals(double x, double y) {
		PolynomialSplineFunction[] top = findTopHorizontalSplines(x, y);
		PolynomialSplineFunction[] bottom = findBottomVerticalSplines(x, y);
		if (top != null && bottom != null) {
			double y1 = top[0].value(x);
			double y2 = bottom[0].value(x);

			return (y - y1) / (y2 - y1) * Math.atan(bottom[1].value(x)) + (y2 - y) / (y2 - y1) * Math.atan(top[1].value(x));
		} else
			return subInterpolator.interpolateHorizontals(x, y);
	}

	private PolynomialSplineFunction[] findBottomVerticalSplines(double x, double y) {
		double value = Double.POSITIVE_INFINITY;
		PolynomialSplineFunction[] result = null;
		for (PolynomialSplineFunction[] function : horizontals) {
			if (function[0].isValidPoint(x)) {
				double polyValue = function[0].value(x);
				if (polyValue < value && polyValue >= y) {
					value = polyValue;
					result = function;
				}
			}
		}
		return result;
	}

	private PolynomialSplineFunction[] findTopHorizontalSplines(double x, double y) {
		double value = Double.NEGATIVE_INFINITY;
		PolynomialSplineFunction[] result = null;
		for (PolynomialSplineFunction[] function : horizontals) {
			if (function[0].isValidPoint(x)) {
				double polyValue = function[0].value(x);
				if (polyValue > value && polyValue <= y) {
					value = polyValue;
					result = function;
				}
			}
		}
		return result;
	}

}
