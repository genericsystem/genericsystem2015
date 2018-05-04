package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.math3.util.Precision;
import org.genericsystem.cv.utils.GPUTools;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Point3;
import org.opencv.core.Scalar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Svd {

	private static final Logger logger = LoggerFactory.getLogger(Svd.class);

	static {
		NativeLibraryLoader.load();
	}

	public static List<Point3> solve(List<Point> srcPts, int[][] rects) {
		List<Point3> pts = new ArrayList<>();
		for (Point point : srcPts)
			pts.add(new Point3(point.x, point.y, 1));

		double[] stdxy = { Math.sqrt(pts.stream().mapToDouble(pt -> pt.x * pt.x).average().getAsDouble()), Math.sqrt(pts.stream().mapToDouble(pt -> pt.y * pt.y).average().getAsDouble()) };

		for (Point3 pt : pts) {
			pt.x /= stdxy[0];
			pt.y /= stdxy[1];
		}
		double xmin = Double.POSITIVE_INFINITY;
		double ymin = Double.POSITIVE_INFINITY;
		double xmax = Double.NEGATIVE_INFINITY;
		double ymax = Double.NEGATIVE_INFINITY;

		for (Point3 pt : pts) {
			if (pt.x < xmin)
				xmin = pt.x;
			if (pt.y < ymin)
				ymin = pt.y;
			if (pt.x > xmax)
				xmax = pt.x;
			if (pt.y > ymax)
				ymax = pt.y;
		}

		double meanspan = Math.max(Math.max(Math.abs(xmin), Math.abs(xmax)), Math.max(Math.abs(ymin), Math.abs(ymax)));

		double lambda = 1 / (meanspan * meanspan);

		int n = pts.size();
		int m = rects.length;

		double[] polarity = { -1, 1, -1, 1 };

		int nDim = 3;
		System.out.println("n = " + n);
		// m * nDim constraints....
		Mat A = new Mat(nDim * m, 3 * n, CvType.CV_64FC1, new Scalar(0));
		// coplanar terms...
		for (int i = 0; i < m; i++) {
			// % rect i: rects(i, 1) --- rects(i, 2)
			// % | |
			// % rects(i, 4) --- rects(i, 3)
			for (int j = 0; j < nDim; j++) {
				int constraint_index = nDim * i + j;
				for (int k = 0; k < 4; k++)
					A.put(constraint_index, 3 * rects[i][k] + j, polarity[k]);
			}
		}

		// data-terms...
		Mat B = new Mat(2 * n, 3 * n, CvType.CV_64FC1, new Scalar(0));
		for (int i = 0; i < n; i++) {
			// % X - x_i Z...
			B.put(2 * i, 3 * i, 1d);
			B.put(2 * i, 3 * i + 2, -pts.get(i).x);
			// % Y - y_i Z...
			B.put(2 * i + 1, 3 * i + 1, 1d);
			B.put(2 * i + 1, 3 * i + 2, -pts.get(i).y);
		}

		// solve the homogenous equation Az = 0
		// [U, D, V] = svd(A + sqrt(lambda) * B);
		// [minSingularValue, minIndex] = min(diag(D));
		Mat dst = GPUTools.gemm(A, A, Core.GEMM_1_T);
		Mat dst2 = GPUTools.gemm(B, B, Core.GEMM_1_T);
		Mat M = GPUTools.addWeighted(dst, 1, dst2, lambda, 0);

		Mat eigenValues = new Mat();
		Mat eigenVectors = new Mat();
		Core.eigen(M, eigenValues, eigenVectors);
		int minIndex = -1;
		for (int i = eigenValues.rows() - 1; i >= 0; i--) {
			if (eigenValues.get(i, 0)[0] > Precision.EPSILON) {
				minIndex = i;
				break;
			}
		}

		Mat result = eigenVectors.row(minIndex);

		double sum = 0;
		for (int i = 0; i < pts.size(); i++)
			sum += result.get(0, 3 * i + 2)[0];

		for (int i = 0; i < pts.size(); i++) {
			pts.get(i).x = sum > 0 ? result.get(0, 3 * i)[0] : -result.get(0, 3 * i)[0];
			pts.get(i).y = sum > 0 ? result.get(0, 3 * i + 1)[0] : -result.get(0, 3 * i + 1)[0];
			pts.get(i).z = sum > 0 ? result.get(0, 3 * i + 2)[0] : -result.get(0, 3 * i + 2)[0];
		}

		// normalize it back
		for (int i = 0; i < pts.size(); i++) {
			pts.get(i).x *= stdxy[0];
			pts.get(i).y *= stdxy[1];
		}

		return pts;
	}
}
