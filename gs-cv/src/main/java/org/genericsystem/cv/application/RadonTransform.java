package org.genericsystem.cv.application;

import org.apache.commons.math3.analysis.interpolation.LinearInterpolator;
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
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
import org.opencv.utils.Converters;
import org.opencv.ximgproc.Ximgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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

		// Mat projectionMap = Mat.zeros(radon.rows(), radon.cols(), CvType.CV_32FC1);
		// Mat map_x = new Mat(radon.size(), CvType.CV_32FC1);
		// Mat map_y = new Mat(radon.size(), CvType.CV_32FC1);
		//
		// for (float col = 0; col < radon.cols(); col++)
		// for (int row = 0; row < radon.rows(); row++) {
		// // float newCol = (int) Math.round((Math.asin(((double) row - radon.rows() / 2) / (col - radon.rows() / 2)) / Math.PI * 180 + minAngle));
		// float newCol = (float) (((double) row - projectionMap.rows() / 2) * Math.sin((col - minAngle) / 180 * Math.PI) + radon.rows() / 2);
		// map_x.put(row, (int) col, newCol);
		// map_y.put(row, (int) col, row);
		// }
		// Imgproc.remap(radon, projectionMap, map_x, map_y, Imgproc.INTER_LINEAR, Core.BORDER_CONSTANT, new Scalar(0, 0, 0));
		// return projectionMap;

	}

	public static Mat fastHoughTransform(Mat vStrip) {
		Mat houghTransform = new Mat();
		Ximgproc.FastHoughTransform(vStrip, houghTransform, CvType.CV_64FC1, Ximgproc.ARO_45_135, Ximgproc.FHT_ADD, Ximgproc.HDO_DESKEW);
		Core.transpose(houghTransform, houghTransform);
		// Core.normalize(houghTransform, houghTransform, 0, 255, Core.NORM_MINMAX);
		return new Mat(houghTransform, new Range(vStrip.width() / 2 - 1, houghTransform.height() - vStrip.width() / 2 - 1), new Range(0, houghTransform.width()));
	}

	@Deprecated
	public static Mat fhtRemap(Mat houghTransform) {
		Mat result = Mat.zeros(houghTransform.rows(), 91, CvType.CV_64FC1);
		// System.out.println(houghTransform);
		for (double col = 0; col < houghTransform.width(); col += 0.25)
			for (int row = 0; row < houghTransform.rows(); row++) {
				int stripSize = (houghTransform.width() + 1) / 2;
				double angle = Math.round(Math.atan((col - stripSize + 1) / (stripSize - 1)) / Math.PI * 180 + 45);
				if (angle < 0 || angle > 90)
					throw new IllegalStateException("Angle : " + angle);
				else
					result.put(row, (int) angle, Math.max(result.get(row, (int) Math.round(angle))[0], houghTransform.get(row, (int) col)[0]));
			}
		return result;
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

	private static class Node {
		double distance = Double.NEGATIVE_INFINITY;
		Map<Integer, Double> children = new HashMap<>();
		Integer parent;
	}

	// Works sometimes…
	public static TrajectStep[] bestTraject2(Mat projectionMap, int minAngle, int maxAngle, double w) {
		int deltaK = 20;
		int n = projectionMap.rows() / deltaK + 1;
		int[] ks = new int[n];
		for (int i = 0; i < n - 1; i++)
			ks[i] = i * deltaK;
		ks[n - 1] = projectionMap.rows() - 1;

		Node[][] nodes = new Node[n][maxAngle - minAngle + 1];
		for (int s = n - 1; s >= 0; s--)
			for (int theta = minAngle; theta < projectionMap.cols() + minAngle; theta++) {
				Node node = new Node();
				nodes[s][theta - minAngle] = node;
				if (s < n - 1) {
					List<Integer> allowedNextAngles = allowedNextAngles(theta, deltaK, minAngle, maxAngle, w);
					for (int nextAngle : allowedNextAngles) {
						double h = (nextAngle - theta) / (double) deltaK;
						double weight = 0;
						for (int t = 0; t < deltaK; t++)
							weight += Math.pow(projectionMap.get(ks[s] + t, (int) Math.round(theta + t * h))[0], 3);
						weight += deltaK * Math.exp(-h * h / 2);
						node.children.put(nextAngle, weight);
						if (s == 0)
							node.distance = 0;
					}
				}
			}

		for (int i = 0; i < ks.length - 1; i++) {
			for (int theta = minAngle; theta <= maxAngle; theta++) {
				Node node = nodes[i][theta - minAngle];
				if (node.distance != Double.NEGATIVE_INFINITY)
					for (Entry<Integer, Double> child : node.children.entrySet()) {
						if (nodes[i + 1][child.getKey() - minAngle].distance < node.distance + child.getValue()) {
							nodes[i + 1][child.getKey() - minAngle].distance = node.distance + child.getValue();
							nodes[i + 1][child.getKey() - minAngle].parent = theta;
						}
					}
			}
		}
		TrajectStep[] bestSteps = new TrajectStep[n];
		int nextTheta = 0;
		double maxDistance = Double.NEGATIVE_INFINITY;
		for (int theta = minAngle; theta <= maxAngle; theta++)
			if (nodes[n - 1][theta - minAngle].distance > maxDistance) {
				maxDistance = nodes[n - 1][theta - minAngle].distance;
				nextTheta = theta;
			}
		bestSteps[n - 1] = new TrajectStep(ks[n - 1], nextTheta, projectionMap.get(ks[n - 1], nextTheta - minAngle)[0]);
		for (int i = n - 2; i >= 0; i--) {
			int theta = nodes[i + 1][nextTheta - minAngle].parent;
			bestSteps[i] = new TrajectStep(ks[i], theta, projectionMap.get(ks[i], theta - minAngle)[0]);
			nextTheta = theta;
		}
		logger.info("Best steps found: {}.", Arrays.asList(bestSteps).stream().map(s -> s.theta).collect(Collectors.toList()));
		return bestSteps;
	}

	// Use a cache if actually used.
	private static List<Integer> allowedNextAngles(int prevTheta, int deltaK, int minAngle, int maxAngle, double w) {
		List<Integer> angles = new ArrayList<>();
		double vA = -w * Math.tan(prevTheta * Math.PI / 180) / 2;
		double vB = -vA;
		double hypothenuse1 = Math.sqrt(w * w / 4 + (deltaK - vA) * (deltaK - vA));
		double maxTheta = 180 * Math.asin((deltaK - vA) / hypothenuse1) / Math.PI;
		double hypothenuse2 = Math.sqrt(w * w / 4 + (deltaK - vB) * (deltaK - vB));
		double minTheta = 180 * Math.asin((vB - deltaK) / hypothenuse2) / Math.PI;
		for (int theta = Math.max(minAngle, (int) Math.ceil(minTheta)); theta <= Math.min(maxAngle, Math.floor(maxTheta)); theta++)
			angles.add(theta);
		return angles;
	}

	public static TrajectStep[] bestTrajectRadon(Mat projectionMap, double anglePenality) {

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
		TrajectStep[] thetas = new TrajectStep[projectionMap.rows()];
		for (int k = projectionMap.rows() - 1; k >= 0; k--) {
			thetas[k] = new TrajectStep(k, prevTheta, projectionMap.get(k, prevTheta)[0]);
			prevTheta = thetaPrev[k][prevTheta];
		}

		return thetas;
	}

	static Mat adaptivHough(Mat houghTransform, int blurSize) {
		Mat adaptivHough = new Mat();
		Mat blur = new Mat();
		Imgproc.blur(houghTransform, blur, new Size(1, blurSize), new Point(-1, -1), Core.BORDER_ISOLATED);
		Core.absdiff(houghTransform, blur, adaptivHough);
		blur.release();
		// adaptivHough.row(0).setTo(new Scalar(0));
		// adaptivHough.row(houghTransform.rows() - 1).setTo(new Scalar(0));
		// Core.pow(adaptivHough, 2, adaptivHough);
		return adaptivHough;
	}

	public static List<HoughTrajectStep> bestTrajectFHT(Mat houghTransform, int blurSize, double anglePenality) {
		int stripWidth = (houghTransform.cols() + 1) / 2;
		Mat adaptivHough = adaptivHough(houghTransform, blurSize);
		List<Double> derivativeToangleCoeff = IntStream.range(0, adaptivHough.cols())
				.mapToObj(derivativeIndex -> Math.abs(Math.atan((double) (derivativeIndex - stripWidth + 1) / (stripWidth - 1)) - Math.atan((double) (derivativeIndex - stripWidth + 2) / (stripWidth - 1))) * 180 / Math.PI).collect(Collectors.toList());
		double[][] score = new double[adaptivHough.rows()][adaptivHough.cols()];
		int[][] thetaPrev = new int[adaptivHough.rows()][adaptivHough.cols()];
		for (int theta = 0; theta < adaptivHough.cols(); theta++)
			score[0][theta] = adaptivHough.get(0, theta)[0];
		for (int k = 1; k < adaptivHough.rows(); k++) {
			for (int derivativeIndex = 0; derivativeIndex < adaptivHough.cols(); derivativeIndex++) {
				double magnitude = adaptivHough.get(k, derivativeIndex)[0];

				double scoreFromPrevTheta = derivativeIndex != 0 ? score[k - 1][derivativeIndex - 1] : Double.NEGATIVE_INFINITY;
				double scoreFromSameTheta = score[k - 1][derivativeIndex];
				double scoreFromNextTheta = derivativeIndex < adaptivHough.cols() - 1 ? score[k - 1][derivativeIndex + 1] : Double.NEGATIVE_INFINITY;

				double bestScore4Pos = -1;
				double prevPenality = derivativeIndex == 0 ? Double.NEGATIVE_INFINITY : anglePenality * derivativeToangleCoeff.get(derivativeIndex - 1);
				double nextPenality = derivativeIndex <= adaptivHough.cols() ? anglePenality * derivativeToangleCoeff.get(derivativeIndex) : Double.NEGATIVE_INFINITY;

				if (scoreFromSameTheta >= (scoreFromPrevTheta + prevPenality) && scoreFromSameTheta >= (scoreFromNextTheta + nextPenality)) {
					bestScore4Pos = scoreFromSameTheta;
					thetaPrev[k][derivativeIndex] = derivativeIndex;
				} else if ((scoreFromPrevTheta + prevPenality) >= scoreFromSameTheta && ((scoreFromPrevTheta + prevPenality) >= (scoreFromNextTheta + nextPenality))) {
					bestScore4Pos = scoreFromPrevTheta + prevPenality;
					thetaPrev[k][derivativeIndex] = derivativeIndex - 1;
				} else {
					bestScore4Pos = scoreFromNextTheta + nextPenality;
					thetaPrev[k][derivativeIndex] = derivativeIndex + 1;
				}
				score[k][derivativeIndex] = magnitude + bestScore4Pos;
			}
		}
		adaptivHough.release();
		return Arrays.asList(buildResult(score, thetaPrev, stripWidth, houghTransform));
	}

	public static List<HoughTrajectStep> bestTrajectFHT2(Mat houghTransform, int blurSize, double penality, int stepCount) {
		int stripWidth = (houghTransform.cols() + 1) / 2;
		Mat adaptivHough = adaptivHough(houghTransform, blurSize);
		double[][] fastAdaptivHough = new double[adaptivHough.rows()][adaptivHough.cols()];
		for (int row = 0; row < adaptivHough.rows(); row++)
			for (int col = 0; col < adaptivHough.cols(); col++)
				fastAdaptivHough[row][col] = adaptivHough.get(row, col)[0];
		List<Double> derivatives = IntStream.range(0, adaptivHough.cols()).mapToObj(theta -> (double) (theta - stripWidth + 1) / (stripWidth - 1)).collect(Collectors.toList());
		double[] angleFromIndex = derivatives.stream().mapToDouble(derivative -> Math.atan(derivative)).toArray();
		double[][] score = new double[stepCount + 1][adaptivHough.cols()];
		int[][] prevDerivatives = new int[stepCount + 1][adaptivHough.cols()];
		for (int theta = 0; theta < adaptivHough.cols(); theta++)
			score[0][theta] = adaptivHough.get(0, theta)[0];
		double step = ((double) adaptivHough.rows() - 1) / stepCount;

		for (int i = 1; i <= stepCount; i++) {
			int row = (int) Math.round(i * step);
			for (int derivativeIndex = 0; derivativeIndex < adaptivHough.cols() - 1; derivativeIndex++) {
				int[] derivativeRange = getPrevRange(derivatives, step, derivatives.get(derivativeIndex), stripWidth);
				int maxPrevDerivative = -1;
				double maxScoreFromPrev = Double.NEGATIVE_INFINITY;
				for (int prevDerivativeIndex = derivativeRange[0]; prevDerivativeIndex < derivativeRange[1]; prevDerivativeIndex++) {
					double scoreFromPrevDerivative = scoreFromSegmentFromPrevDerivative(score[i - 1][prevDerivativeIndex], fastAdaptivHough, angleFromIndex, penality, row, (int) ((i - 1) * step), derivativeIndex, prevDerivativeIndex);
					if (maxScoreFromPrev < scoreFromPrevDerivative) {
						maxScoreFromPrev = scoreFromPrevDerivative;
						maxPrevDerivative = prevDerivativeIndex;
					}
				}
				prevDerivatives[i][derivativeIndex] = maxPrevDerivative;
				score[i][derivativeIndex] = maxScoreFromPrev;
			}
		}
		adaptivHough.release();
		return Arrays.asList(buildResult2(derivatives, score, prevDerivatives, stripWidth, houghTransform, step));
	}

	private static HoughTrajectStep[] buildResult2(List<Double> derivativeFromIndex, double[][] score, int[][] thetaPrev, int stripWidth, Mat houghTransform, double step) {
		int prevTheta = getBestScoreTheta(score);
		HoughTrajectStep[] result = new HoughTrajectStep[score.length];
		for (int i = score.length - 1; i >= 0; i--) {
			int row = (int) Math.round(i * step);
			double derivative = derivativeFromIndex.get(prevTheta);
			result[i] = new HoughTrajectStep(row, derivative, houghTransform.get(row, prevTheta)[0]);
			prevTheta = thetaPrev[i][prevTheta];
		}
		return result;
	}

	private static HoughTrajectStep[] buildResult(double[][] score, int[][] thetaPrev, int stripWidth, Mat houghTransform) {
		int prevTheta = getBestScoreTheta(score);
		HoughTrajectStep[] result = new HoughTrajectStep[score.length];
		for (int y = score.length - 1; y >= 0; y--) {
			double derivative = ((double) prevTheta - (stripWidth - 1)) / (stripWidth - 1);
			result[y] = new HoughTrajectStep(y, derivative, houghTransform.get(y, prevTheta)[0]);
			prevTheta = thetaPrev[y][prevTheta];
		}
		return result;
	}

	private static double scoreFromSegmentFromPrevDerivative(double prevScore, double[][] fastAdaptivHough, double[] anglesFromIndex, double penality, int row, int prevRow, int derivativeIndex, int prevDerivativeIndex) {
		double deltaDerivativeIndexByRow = ((double) derivativeIndex - (double) prevDerivativeIndex) / (row - prevRow);
		double x = prevDerivativeIndex + deltaDerivativeIndexByRow;
		for (int currentRow = prevRow + 1; currentRow <= row; currentRow++) {
			prevScore += fastAdaptivHough[currentRow][(int) Math.round(x)];
			x += deltaDerivativeIndexByRow;
		}
		return prevScore += penality * Math.exp(-0.5 * Math.pow(anglesFromIndex[derivativeIndex] - anglesFromIndex[prevDerivativeIndex], 2));
	}

	private static int[] getPrevRange(List<Double> derivativeFromIndex, double step, double derivative, int stripWidth) {

		double y1 = step + -derivative * stripWidth / 2;
		double y2 = step + derivative * stripWidth / 2;
		List<Integer> range = new ArrayList<>();
		for (int derivativePrevIndex = 0; derivativePrevIndex < (2 * stripWidth - 1); derivativePrevIndex++) {
			double prevDerivative = derivativeFromIndex.get(derivativePrevIndex);
			double y1Prev = -prevDerivative * stripWidth / 2;
			double y2Prev = prevDerivative * stripWidth / 2;
			if (y1Prev < y1 && y2Prev < y2)
				range.add(derivativePrevIndex);
		}
		// System.out.println(derivative + " " + Arrays.toString(new int[] { range.get(0), range.get(range.size() - 1) }));
		return new int[] { range.get(0), range.get(range.size() - 1) };
	}

	private static int getBestScoreTheta(double[][] score) {
		double maxScore = Double.NEGATIVE_INFINITY;
		int maxTheta = -1;
		for (int theta = 0; theta < score[0].length; theta++) {
			double lastScore = score[score.length - 1][theta];
			if (lastScore > maxScore) {
				maxScore = lastScore;
				maxTheta = theta;
			}
		}
		return maxTheta;
	}

	public static List<Mat> extractStrips(Mat src, int stripNumber, double stripWidth, double step) {
		List<Mat> strips = new ArrayList<>();
		for (int stripIndex = 0; stripIndex < stripNumber; stripIndex++)
			strips.add(extractStrip(src, (int) Math.round(stripIndex * step), (int) stripWidth));
		return strips;
	}

	public static Mat extractStrip(Mat src, int startX, int width) {
		return new Mat(src, new Range(0, src.rows()), new Range(startX, startX + width));
	}

	public static List<PolynomialSplineFunction> estimateBaselines(Mat image, double anglePenalty, int minAngle, int maxAngle, int yStep) {

		int n = 20;// Number of overlapping vertical strips.
		float r = .5f;// Overlap ratio between two consecutive strips.
		double w = (image.width() / (n * (1 - r) + r));// w = width of a vertical strip.
		double step = (int) ((1 - r) * w);// Image width = [n(1 - r) + r] w

		int x = 0;
		List<Function<Double, Double>> approxFunctions = new ArrayList<>();
		Mat preprocessed = new Img(image, false).adaptativeGaussianInvThreshold(5, 3).getSrc();
		for (int i = 0; i < n; i++) {
			Mat radonTransform = radonTransform(extractStrip(preprocessed, x, (int) w), minAngle, maxAngle);
			Mat projMap = radonRemap(radonTransform, minAngle);
			Imgproc.morphologyEx(projMap, projMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 4)));
			TrajectStep[] angles = bestTrajectRadon(projMap, anglePenalty);
			// TrajectStep[] angles = bestTraject2(projMap, minAngle, maxAngle, w);
			projMap.release();
			radonTransform.release();
			approxFunctions.add(approxTraject(angles));
			x += step;
		}

		return toPolynomialSplineFunction(approxFunctions, image.size(), yStep, minAngle, n, r);
	}

	static List<PolynomialSplineFunction> toPolynomialSplineFunction(List<Function<Double, Double>> approxFunctions, Size imageSize, int yMeshStep, int minAngle, int vStripsNumber, float recover) {
		double w = (imageSize.width / (vStripsNumber * (1 - recover) + recover));
		double xStep = (int) ((1 - recover) * w);

		// 0, center of each vertical strip, image.width() - 1
		double[] xs = new double[vStripsNumber + 2];

		for (int i = 0; i < vStripsNumber; i++)
			xs[i + 1] = i * xStep + w / 2;

		xs[vStripsNumber + 1] = imageSize.width - 1;
		int lines = (int) ((imageSize.height - 1) / yMeshStep + 1);
		List<PolynomialSplineFunction> hLines = new ArrayList<>();
		for (int line = 0; line < lines; line++) {
			double[] ys = new double[vStripsNumber + 2];
			ys[vStripsNumber / 2] = line * yMeshStep + yMeshStep / 2;
			for (int j = vStripsNumber / 2; j <= vStripsNumber; j++) {
				double theta = (approxFunctions.get(j - 1).apply(ys[j]) + minAngle) / 180 * Math.PI;
				ys[j + 1] = ys[j] + xStep * Math.tan(theta);
			}
			for (int j = vStripsNumber / 2; j > 0; j--) {
				double theta = (approxFunctions.get(j - 1).apply(ys[j]) + minAngle) / 180 * Math.PI;
				ys[j - 1] = ys[j] - xStep * Math.tan(theta);
			}
			hLines.add(new LinearInterpolator().interpolate(xs, ys));
		}
		return hLines;
	}

	public static Function<Double, Double> approxTraject(TrajectStep[] traj) {
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

	// public static List<OrientedPoint> toHorizontalOrientedPoints(Function<Double, Double> f, double x, int height, int hStep) {
	// List<OrientedPoint> orientedPoints = new ArrayList<>();
	// for (int y = hStep; y + hStep <= height; y += hStep) {
	// double angle = (f.apply((double) y) - 45) / 180 * Math.PI;
	// orientedPoints.add(new OrientedPoint(new Point(x, y), angle, 1));
	// }
	// return orientedPoints;
	// }
	//
	// public static List<OrientedPoint> toVerticalOrientedPoints(Function<Double, Double> f, double y, int width, int vStep) {
	// List<OrientedPoint> orientedPoints = new ArrayList<>();
	// for (int x = vStep; x + vStep <= width; x += vStep) {
	// double angle = -(f.apply((double) x) - 45) / 180 * Math.PI;
	// orientedPoints.add(new OrientedPoint(new Point(x, y), angle, 1));
	// }
	// return orientedPoints;
	// }

	public static List<OrientedPoint>[] toHorizontalOrientedPoints(List<HoughTrajectStep> trajectSteps, double x, double localTheshold, double globalTheshold) {
		List<OrientedPoint> topPoints = new ArrayList<>();
		List<OrientedPoint> bottomPoints = new ArrayList<>();
		List<HoughTrajectStep[]> lines = getStripLinesFHT(trajectSteps, localTheshold, globalTheshold);
		lines.stream().forEach(trajectStep -> {
			topPoints.add(new OrientedPoint(new Point(x, trajectStep[0].y), (trajectStep[0].getTheta() - 45) / 180 * Math.PI, trajectStep[0].magnitude, trajectStep[0].derivative));
			bottomPoints.add(new OrientedPoint(new Point(x, trajectStep[1].y), (trajectStep[1].getTheta() - 45) / 180 * Math.PI, trajectStep[1].magnitude, trajectStep[1].derivative));
		});
		return new List[] { topPoints, bottomPoints };
	}

	public static List<OrientedPoint>[] toVerticalOrientedPoints(List<HoughTrajectStep> trajectSteps, double y, double localTheshold, double globalTheshold) {
		List<OrientedPoint> leftPoints = new ArrayList<>();
		List<OrientedPoint> rightPoints = new ArrayList<>();

		List<HoughTrajectStep[]> lines = getStripLinesFHT(trajectSteps, localTheshold, globalTheshold);

		lines.stream().forEach(trajectStep -> {
			leftPoints.add(new OrientedPoint(new Point(trajectStep[0].y, y), -(trajectStep[0].getTheta() - 45) / 180 * Math.PI, trajectStep[0].magnitude, trajectStep[0].derivative));
			rightPoints.add(new OrientedPoint(new Point(trajectStep[1].y, y), -(trajectStep[1].getTheta() - 45) / 180 * Math.PI, trajectStep[1].magnitude, trajectStep[1].derivative));
		});
		return new List[] { leftPoints, rightPoints };
	}

	public static void displayHSplines(List<PolynomialSplineFunction> vRadonSplinesFunctions, Mat image) {
		for (int col = 0; col < image.width(); col++)
			for (PolynomialSplineFunction f : vRadonSplinesFunctions) {
				int row = (int) f.value(col);
				if (row >= 0 && row < image.rows())
					image.put(row, col, 0d, 255d, 0d);
			}
	}

	public static void displayVSplines(List<PolynomialSplineFunction> hRadonSplinesFunctions, Mat image) {
		for (int row = 0; row < image.height(); row++)
			for (PolynomialSplineFunction f : hRadonSplinesFunctions) {
				int col = (int) f.value(row);
				if (col >= 0 && col < image.cols())
					image.put(row, col, 0d, 255d, 0d);
			}
	}

	public static List<Pair> getLocalExtr(Mat fht, double minWeight) {
		List<Pair> weightedPoints = new ArrayList<>();
		for (int y = 0; y < fht.rows(); ++y) {
			List<Double> pLine = new ArrayList<>();
			Converters.Mat_to_vector_double(fht.row(Math.max(y - 1, 0)).t(), pLine);
			List<Double> cLine = new ArrayList<>();
			Converters.Mat_to_vector_double(fht.row(Math.max(y, 0)).t(), cLine);
			List<Double> nLine = new ArrayList<>();
			Converters.Mat_to_vector_double(fht.row(Math.min(y + 1, fht.rows() - 1)).t(), nLine);
			for (int x = 0; x < fht.cols(); ++x) {
				double value = cLine.get(x);
				if (value >= minWeight) {
					int[] isLocalMax = new int[] { 0 };
					for (int xx = Math.max(x - 2, 0); xx <= Math.min(x + 2, fht.cols() - 1); ++xx) {
						if (!incIfGreater(value, pLine.get(xx), isLocalMax) || !incIfGreater(value, cLine.get(xx), isLocalMax) || !incIfGreater(value, nLine.get(xx), isLocalMax)) {
							isLocalMax[0] = 0;
							break;
						}
					}
					if (isLocalMax[0] > 0)
						weightedPoints.add(new Pair(value, new Point(x, y)));
				}
			}
		}
		return weightedPoints;
	}

	private static boolean incIfGreater(double a, double b, int[] value) {
		if (a < b)
			return false;
		if (a > b)
			++value[0];
		return true;
	}

	static class Pair {

		double value;
		Point point;

		private Pair(double value, Point point) {
			this.value = value;
			this.point = point;
		}
	}

	public static List<HoughTrajectStep[]> getStripLinesFHT(List<HoughTrajectStep> magnitudes, double localTheshold, double globalTheshold) {
		Set<HoughTrajectStep> alreadyComputed = new HashSet<>();
		double max = magnitudes.stream().mapToDouble(ts -> ts.magnitude).max().getAsDouble();
		// System.out.println("Max : " + max);
		List<HoughTrajectStep[]> result = new ArrayList<>();
		for (HoughTrajectStep trajectStep : magnitudes.stream().sorted().collect(Collectors.toList())) {
			if (trajectStep.magnitude < globalTheshold * max)
				break;
			if (!alreadyComputed.contains(trajectStep)) {
				double tAlpha = localTheshold * trajectStep.magnitude;

				int y1 = trajectStep.y;
				while (y1 >= 0 && magnitudes.get(y1).magnitude >= tAlpha)
					y1--;
				if (y1 != 0)
					y1++;

				int y2 = trajectStep.y;
				while (y2 < magnitudes.size() && magnitudes.get(y2).magnitude >= tAlpha)
					y2++;
				if (y2 != magnitudes.size() - 1)
					y2--;

				boolean alreadyVisited = false;
				for (int y = y1; y <= y2; y++)
					if (alreadyComputed.contains(magnitudes.get(y))) {
						alreadyVisited = true;
						break;
					}
				for (int y = y1; y <= y2; y++)
					alreadyComputed.add(magnitudes.get(y));
				if (!alreadyVisited)
					result.add(new HoughTrajectStep[] { magnitudes.get(y1), magnitudes.get(y2) });
			}
		}
		return result;
	}

}
