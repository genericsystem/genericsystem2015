package org.genericsystem.cv;

import java.util.Arrays;

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
		double[][] pts = { { -2, 1, 1 }, { 0, 1, 1 }, { 1, 1, 1 }, { -1, 0, 1 }, { 0, 0, 1 }, { 1, 0, 1 }, { -1, -1, 1 }, { 0, -1, 1 }, { 1, -1, 1 } };

		int[][] rects = { { 0, 1, 4, 3 }, { 1, 2, 5, 4 }, { 2, 3, 7, 6 }, { 4, 5, 7, 8 } };

		double[][] result = solve(pts, rects);
		System.out.println(Arrays.deepToString(result));

		// double[][] datas = { { pts[0][0], -pts[1][0], 0, -pts[3][0], pts[4][0], 0, 0, 0, 0 }, { 0, pts[1][0], -pts[2][0], 0, -pts[4][0], pts[5][0], 0, 0, 0 }, { 0, 0, 0, pts[3][0], -pts[4][0], 0, -pts[6][0], pts[7][0], 0 },
		// { 0, 0, 0, 0, pts[4][0], -pts[5][0], 0, -pts[7][0], pts[8][0] },
		//
		// { pts[0][1], -pts[1][1], 0, -pts[3][1], pts[4][1], 0, 0, 0, 0 }, { 0, pts[1][1], -pts[2][1], 0, -pts[4][1], pts[5][1], 0, 0, 0 }, { 0, 0, 0, pts[3][1], -pts[4][1], 0, -pts[6][1], pts[7][1], 0 },
		// { 0, 0, 0, 0, pts[4][1], -pts[5][1], 0, -pts[7][1], pts[8][1] },
		//
		// { pts[0][2], -pts[1][2], 0, -pts[3][2], pts[4][2], 0, 0, 0, 0 }, { 0, pts[1][2], -pts[2][2], 0, -pts[4][2], pts[5][2], 0, 0, 0 }, { 0, 0, 0, pts[3][2], -pts[4][2], 0, -pts[6][2], pts[7][2], 0 },
		// { 0, 0, 0, 0, pts[4][2], -pts[5][2], 0, -pts[7][2], pts[8][2] } };

		// double[] coeffs = new double[datas.length];
		// Arrays.fill(coeffs, 1 / Math.sqrt(9));
		// double zs[] = solve(datas, coeffs);
		// System.out.println(Arrays.toString(zs));
		// System.out.println(Math.sqrt(Arrays.stream(zs).map(d -> d * d).sum()));
		// System.out.println(Arrays.toString(mul(datas, zs)));
		// int i = 0;
		// for (double[] pt : pts) {
		// System.out.println(zs[i] * pt[0] + ", " + zs[i] * pt[1] + ", " + zs[i] * pt[2]);
		// i++;
		// }
	}

	public static double[][] solve(double[][] srcPts, int[][] rects) {
		double[][] pts = srcPts.clone();
		// options = argutil_setdefaults(options, 'lambda', [], 'z_constraint', true, 'scale', 1);

		double[] stdxy = { Math.sqrt(Arrays.stream(pts).mapToDouble(pt -> pt[0] * pt[0]).average().getAsDouble()), Math.sqrt(Arrays.stream(pts).mapToDouble(pt -> pt[1] * pt[1]).average().getAsDouble()) };

		for (double[] pt : pts) {
			pt[0] /= stdxy[0];
			pt[1] /= stdxy[1];
		}
		double xmin = Double.MAX_VALUE;
		double ymin = Double.MAX_VALUE;
		double xmax = Double.MIN_VALUE;
		double ymax = Double.MIN_VALUE;

		for (double[] pt : pts) {
			if (pt[0] < xmin)
				xmin = pt[0];
			if (pt[1] < ymin)
				ymin = pt[1];
			if (pt[0] > xmax)
				xmax = pt[0];
			if (pt[1] < ymax)
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
		Mat result = eigenVectors.col(eigenVectors.cols() - 1);
		// [V, D] = eigs(M, 1, 'SM');
		// [minEigValue, minIndex] = min(diag(D));
		// sol = V(:, minIndex);

		Mat sum = new Mat();
		Core.reduce(result, sum, 0, Core.REDUCE_SUM);

		double sum_ = sum.get(0, 0)[0];

		for (int i = 0; i < pts.length; i++)
			pts[i][2] = sum_ > 0 ? -result.get(i, 0)[0] : result.get(i, 0)[0];

		// % normalze it back
		for (int i = 0; i < pts.length; i++) {
			pts[i][0] = pts[i][0] * stdxy[0];
			pts[i][1] = pts[i][1] * stdxy[1];
		}

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

	static double[] solve(double[][] datas, double[] coeffs) {
		Mat li_set = new Mat(9, datas.length, CvType.CV_64FC1);
		Mat tau = new Mat(datas.length, datas.length, CvType.CV_64FC1, new Scalar(0, 0, 0));
		int i = 0;
		for (double[] data : datas) {
			int j = 0;
			for (double dat : data) {
				li_set.put(j, i, dat);
				j++;
			}
			tau.put(i, i, coeffs[i]);
			i++;
		}
		Mat L = li_set.t();
		Mat ATA = new Mat();
		Mat dst = new Mat();

		Core.gemm(L.t(), tau.t(), 1, new Mat(), 0, dst);
		Core.gemm(dst, tau, 1, new Mat(), 0, dst);
		Core.gemm(dst, L, 1, new Mat(), 0, ATA);

		// Obtain eigendecomposition
		Mat v = new Mat();
		Core.SVDecomp(ATA, new Mat(), v, new Mat());

		// Check eigenvecs after SVDecomp
		if (v.rows() < 9)
			throw new IllegalStateException();

		// normalize ??
		return new double[] { v.get(0, 8)[0], v.get(1, 8)[0], v.get(2, 8)[0], v.get(3, 8)[0], v.get(4, 8)[0], v.get(5, 8)[0], v.get(6, 8)[0], v.get(7, 8)[0], v.get(8, 8)[0] };
	}

}
