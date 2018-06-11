package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.genericsystem.cv.lm.LevenbergImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.core.Rect;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RadonTransform {

	static {
		NativeLibraryLoader.load();
	}

	private static final Logger logger = LoggerFactory.getLogger(RadonTransform.class);

	public static Mat radonTransform(Mat src, int minAngle, int maxAngle) {
		Mat dst = Mat.zeros(src.rows(), src.rows(), CvType.CV_64FC1);
		int center = dst.rows() / 2;
		src.convertTo(new Mat(dst, new Rect(new Point(center - src.cols() / 2, 0), new Point(center + src.cols() / 2, src.rows()))), CvType.CV_64FC1);
		Mat radon = Mat.zeros(dst.rows(), -minAngle + maxAngle + 1, CvType.CV_64FC1);
		for (int t = minAngle; t <= maxAngle; t++) {
			Mat rotated = new Mat();
			Mat rotation = Imgproc.getRotationMatrix2D(new Point(center, center), t, 1);
			Imgproc.warpAffine(dst, rotated, rotation, new Size(dst.cols(), dst.rows()), Imgproc.INTER_NEAREST);
			Core.reduce(rotated, radon.col(t - minAngle), 1, Core.REDUCE_SUM);
			rotated.release();
			rotation.release();
		}
		dst.release();
		Core.normalize(radon, radon, 0, 255, Core.NORM_MINMAX);
		return radon;
	}

	public static Mat radonRemap(Mat radon, int minAngle) {
		Mat projectionMap = Mat.zeros(radon.rows(), radon.cols(), CvType.CV_64FC1);
		for (int k = 0; k < projectionMap.rows(); k++) {
			for (int tetha = 0; tetha < projectionMap.cols(); tetha++) {
				int p = (int) ((k - projectionMap.rows() / 2) * Math.sin(((double) tetha - minAngle) / 180 * Math.PI) + radon.rows() / 2);
				projectionMap.put(k, tetha, Math.max(projectionMap.get(k, tetha)[0], radon.get(p, tetha)[0]));
			}
		}
		return projectionMap;
	}

	public static RadonTrajectStep[] bestTrajectRadon(Mat projectionMap, double anglePenality) {

		double[][] score = new double[projectionMap.rows()][projectionMap.cols()];
		int[][] thetaPrev = new int[projectionMap.rows()][projectionMap.cols()];
		for (int theta = 0; theta < projectionMap.cols(); theta++)
			score[0][theta] = projectionMap.get(0, theta)[0];
		for (int k = 1; k < projectionMap.rows(); k++) {
			for (int theta = 0; theta < projectionMap.cols(); theta++) {
				double magnitude = projectionMap.get(k, theta)[0];

				double scoreFromPrevTheta = theta != 0 ? score[k - 1][theta - 1] : Double.NEGATIVE_INFINITY;
				double scoreFromSameTheta = score[k - 1][theta];
				double scoreFromNextTheta = theta < projectionMap.cols() - 1 ? score[k - 1][theta + 1] : Double.NEGATIVE_INFINITY;

				double bestScore4Pos = -1;
				double prevPenality = theta <= 0 ? Double.NEGATIVE_INFINITY : anglePenality;
				double nextPenality = theta < projectionMap.cols() ? anglePenality : Double.NEGATIVE_INFINITY;

				if (scoreFromSameTheta >= (scoreFromPrevTheta + prevPenality) && scoreFromSameTheta >= (scoreFromNextTheta + nextPenality)) {
					bestScore4Pos = scoreFromSameTheta;
					thetaPrev[k][theta] = theta;
				} else if ((scoreFromPrevTheta + prevPenality) >= scoreFromSameTheta && ((scoreFromPrevTheta + prevPenality) >= (scoreFromNextTheta + nextPenality))) {
					bestScore4Pos = scoreFromPrevTheta + prevPenality;
					thetaPrev[k][theta] = theta - 1;
				} else {
					bestScore4Pos = scoreFromNextTheta + nextPenality;
					thetaPrev[k][theta] = theta + 1;
				}
				score[k][theta] = magnitude + bestScore4Pos;
			}
		}

		double maxScore = Double.NEGATIVE_INFINITY;
		int prevTheta = -1;
		for (int theta = 0; theta < projectionMap.cols(); theta++) {
			double lastScore = score[projectionMap.rows() - 1][theta];
			if (lastScore > maxScore) {
				maxScore = lastScore;
				prevTheta = theta;
			}
		}
		assert prevTheta != -1;
		RadonTrajectStep[] thetas = new RadonTrajectStep[projectionMap.rows()];
		for (int k = projectionMap.rows() - 1; k >= 0; k--) {
			thetas[k] = new RadonTrajectStep(k, prevTheta, projectionMap.get(k, prevTheta)[0]);
			prevTheta = thetaPrev[k][prevTheta];
		}

		return thetas;
	}

	// public static Function<Double, Double> approxTraject(Mat houghTransform) {
	// List<double[]> values = new ArrayList<>();
	// for (int row = 0; row < houghTransform.rows(); row++) {
	// List<Double> houghLine = new ArrayList<>();
	// Converters.Mat_to_vector_double(houghTransform.row(row).t(), houghLine);
	// houghLine.add((double) row);
	// values.add(houghLine.toArray(new double[houghLine.size() - 1]));
	// }
	// BiFunction<Double, double[], Double> f = (x, params) -> params[0] + params[1] * x + params[2] * x * x + params[3] * x * x * x + params[4] * x * x * x * x + params[5] * x * x * x * x * x;
	// BiFunction<double[], double[], Double> error = (xy, params) -> {
	// double[] magnitudes = new double[houghTransform.rows()];
	// for (int row = 0; row < houghTransform.rows(); row++)
	// magnitudes[row] = params[(int) Math.round(f.apply((double) row, params))];
	//
	// double average = Arrays.stream(magnitudes).average().getAsDouble();
	// double variance = 0;
	//
	// return Math.sqrt(variance);
	// };
	//
	// double[] params = new LevenbergImpl<>(error, values, new double[] { 0, 0, 0, 0, 0, 0 }).getParams();
	// return x -> f.apply(x, params);
	// }

	public static Mat extractStrip(Mat src, int startX, int width) {
		return new Mat(src, new Range(0, src.rows()), new Range(startX, startX + width));
	}

	public static Function<Double, Double> approxTraject(RadonTrajectStep[] traj) {
		List<double[]> values = new ArrayList<>();
		for (int k = 0; k < traj.length; k++)
			values.add(new double[] { k, traj[k].theta, traj[k].magnitude });
		double firstTheta = traj[0].theta;
		double k = traj.length - 1;
		double lastTheta = traj[traj.length - 1].theta;
		BiFunction<Double, double[], Double> f = (x, params) -> firstTheta + ((lastTheta - firstTheta - params[0] * k * k - params[1] * k * k * k - params[2] * k * k * k) / k) * x + params[0] * x * x + params[1] * x * x * x + params[2] * x * x * x * x;
		BiFunction<double[], double[], Double> error = (xy, params) -> (f.apply(xy[0], params) - xy[1]) * xy[2];
		double[] params = new LevenbergImpl<>(error, values, new double[] { 0, 0, 0 }).getParams();
		return x -> f.apply(x, params);
	}

	public static class RadonTrajectStep implements Comparable<RadonTrajectStep> {
		public final int k;
		public int theta;
		public final double magnitude;

		public RadonTrajectStep(int k, int theta, double magnitude) {
			this.k = k;
			this.theta = theta;
			this.magnitude = magnitude;
		}

		@Override
		public int compareTo(RadonTrajectStep step) {
			return Double.compare(step.magnitude, magnitude);
		}

	}

}
