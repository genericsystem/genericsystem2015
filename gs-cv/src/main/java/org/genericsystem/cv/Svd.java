package org.genericsystem.cv;

import java.util.Arrays;

import org.apache.commons.math3.util.Precision;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;

public class Svd {
	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		double[][] pts = { { 2, 4, 1 }, { 3, 4, 1 }, { 4, 4, 1 }, { 2, 3, 1 }, { 3, 3, 1 }, { 4, 3, 1 }, { 2, 2, 1 }, { 3, 2, 1 }, { 4, 2, 1 } };

		int[][] rects = { { 3, 4, 1, 0 }, { 4, 5, 2, 1 }, { 6, 7, 4, 3 }, { 7, 8, 5, 4 } };

		double[][] result = solve(pts, rects);
		System.out.println(Arrays.deepToString(result));

	}

	public static double[][] solve(double[][] srcPts, int[][] rects) {
		double[][] pts = new double[srcPts.length][srcPts[0].length];
		for (int i = 0; i < srcPts.length; i++)
			pts[i] = Arrays.copyOf(srcPts[i], srcPts[i].length);
		// options = argutil_setdefaults(options, 'lambda', [], 'z_constraint', true, 'scale', 1);

		double[] stdxy = { Math.sqrt(Arrays.stream(pts).mapToDouble(pt -> pt[0] * pt[0]).average().getAsDouble()), Math.sqrt(Arrays.stream(pts).mapToDouble(pt -> pt[1] * pt[1]).average().getAsDouble()) };

		System.out.println(Arrays.toString(stdxy));

		for (double[] pt : pts) {
			pt[0] /= stdxy[0];
			pt[1] /= stdxy[1];
		}
		double xmin = Double.POSITIVE_INFINITY;
		double ymin = Double.POSITIVE_INFINITY;
		double xmax = Double.NEGATIVE_INFINITY;
		double ymax = Double.NEGATIVE_INFINITY;

		for (double[] pt : pts) {
			if (pt[0] < xmin)
				xmin = pt[0];
			if (pt[1] < ymin)
				ymin = pt[1];
			if (pt[0] > xmax)
				xmax = pt[0];
			if (pt[1] > ymax)
				ymax = pt[1];
		}

		double meanspan = Math.max(Math.max(Math.abs(xmin), Math.abs(xmax)), Math.max(Math.abs(ymin), Math.abs(ymax)));

		double lambda = 1 / (meanspan * meanspan);

		int n = pts.length;
		int m = rects.length;

		double[] polarity = { -1, 1, -1, 1 };

		int nDim = 3;
		System.out.println("n = " + n);
		// % m * nDim constraints....
		Mat A = new Mat(nDim * m, 3 * n, CvType.CV_64FC1, new Scalar(0));
		// % coplanar terms...
		for (int i = 0; i < m; i++) {
			// % rect i: rects(i, 1) --- rects(i, 2)
			// % | |
			// % rects(i, 4) --- rects(i, 3)
			for (int j = 0; j < nDim; j++) {
				int constraint_index = nDim * i + j;
				for (int k = 0; k < 4; k++)
					A.put(constraint_index, (3 * rects[i][k]) + j, polarity[k]);
			}
		}

		for (int row = 0; row < A.rows(); row++) {
			for (int col = 0; col < A.cols(); col++) {
				System.out.print(A.get(row, col)[0] + " ");
			}
			System.out.println();
		}
		System.out.println();
		Mat B = new Mat(2 * n, 3 * n, CvType.CV_64FC1, new Scalar(0));
		// % data-terms...
		for (int i = 0; i < n; i++) {
			// % X - x_i Z...
			B.put(2 * i, 3 * i, 1d);
			B.put(2 * i, 3 * i + 2, -pts[i][0]);
			// % Y - y_i Z...
			B.put(2 * i + 1, 3 * i + 1, 1d);
			B.put(2 * i + 1, 3 * i + 2, -pts[i][1]);
		}

		for (int row = 0; row < B.rows(); row++) {
			for (int col = 0; col < B.cols(); col++) {
				System.out.print(B.get(row, col)[0] + " ");
			}
			System.out.println();
		}
		System.out.println();

		// % solve the homogenous equation Az = 0
		// % [U, D, V] = svd(A + sqrt(lambda) * B);
		// [minSingularValue, minIndex] = min(diag(D));
		Mat dst = new Mat();
		Core.gemm(A.t(), A, 1, new Mat(), 0, dst);
		Mat dst2 = new Mat();

		Core.gemm(B.t(), B, 1, new Mat(), 0, dst2);
		Mat M = new Mat();

		Core.addWeighted(dst, 1, dst2, lambda, 0, M);

		// Mat M = A.t() * A + lambda * B.t() * B;

		Mat eigenValues = new Mat();
		Mat eigenVectors = new Mat();
		Core.eigen(M, eigenValues, eigenVectors);

		int minIndex = -1;
		double minValue = Double.POSITIVE_INFINITY;
		for (int i = 0; i < eigenValues.rows(); i++) {
			if (eigenValues.get(i, 0)[0] > Precision.EPSILON && eigenValues.get(i, 0)[0] < minValue) {
				minValue = eigenValues.get(i, 0)[0];
				minIndex = i;
			}
		}
		Mat result = eigenVectors.row(minIndex);
		// [V, D] = eigs(M, 1, 'SM');
		// [minEigValue, minIndex] = min(diag(D));
		// sol = V(:, minIndex);

		double sum = 0;
		for (int i = 0; i < pts.length; i++)
			sum += result.get(0, 3 * i + 2)[0];

		for (int i = 0; i < pts.length; i++) {
			pts[i][0] = sum > 0 ? -result.get(0, 3 * i)[0] : result.get(0, 3 * i)[0];
			pts[i][1] = sum > 0 ? -result.get(0, 3 * i + 1)[0] : result.get(0, 3 * i + 1)[0];
			pts[i][2] = sum > 0 ? -result.get(0, 3 * i + 2)[0] : result.get(0, 3 * i + 2)[0];
		}

		// % normalze it back
		for (int i = 0; i < pts.length; i++) {
			pts[i][0] *= stdxy[0];
			pts[i][1] *= stdxy[1];
		}

		// Core.gemm(A, result, 1, new Mat(), 0, error);
		// for (int row = 0; row < error.rows(); row++) {
		// for (int col = 0; col < error.cols(); col++) {
		// System.out.print(error.get(row, col)[0] + " ");
		// }
		// System.out.println();
		// }
		// System.out.println();
		//
		// for (int row = 0; row < result.rows(); row++) {
		// System.out.println("=== " + result.get(row, 0)[0]);
		// }

		return pts;

	}

	static double[] mul(double[][] datas, double[] sol) {
		double[] result = new double[datas.length];
		for (int i = 0; i < datas.length; i++) {
			for (int j = 0; j < datas[i].length; j++) {
				result[i] += datas[i][j];
			}
		}
		return result;
	}
}
