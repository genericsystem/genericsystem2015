package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.ximgproc.Ximgproc;

public class FHT {

	public static List<Mat> extractStrips(Mat src, int stripNumber, double stripSize, double step) {
		Mat enlargedBinarized = new Mat();
		Core.copyMakeBorder(src, enlargedBinarized, 0, 0, (int) Math.round(stripSize / 2), (int) Math.round(stripSize / 2), Core.BORDER_CONSTANT, new Scalar(0));
		List<Mat> strips = new ArrayList<>();
		for (int stripIndex = 0; stripIndex < stripNumber; stripIndex++)
			strips.add(extractStrip(enlargedBinarized, (int) Math.round(stripIndex * step), (int) stripSize));
		return strips;
	}

	public static Mat extractStrip(Mat src, int startX, int width) {
		return new Mat(src, new Range(0, src.rows()), new Range(startX, startX + width));
	}

	public static Mat fastHoughTransform(Mat strip) {
		Mat houghTransform = new Mat();
		Ximgproc.FastHoughTransform(strip, houghTransform, CvType.CV_64FC1, Ximgproc.ARO_45_135, Ximgproc.FHT_ADD, Ximgproc.HDO_DESKEW);
		Core.transpose(houghTransform, houghTransform);
		Core.normalize(houghTransform, houghTransform, 0, 1, Core.NORM_MINMAX);
		return new Mat(houghTransform, new Range(strip.width() / 2 - 1, houghTransform.height() - strip.width() / 2 - 1), new Range(0, houghTransform.width()));
	}

	static Mat adaptivHough(Mat houghTransform, int blurSize) {
		Mat adaptivHough = new Mat();
		Mat blur = new Mat();
		Imgproc.blur(houghTransform, blur, new Size(1, blurSize), new Point(-1, -1), Core.BORDER_ISOLATED);
		Core.absdiff(houghTransform, blur, adaptivHough);
		blur.release();
		// Core.normalize(adaptivHough, adaptivHough, 0, 255, Core.NORM_MINMAX);
		return adaptivHough;
	}

	public static List<TrajectStep> bestTrajectFHT(Mat houghTransform, int blurSize, double anglePenality) {
		int stripWidth = (houghTransform.cols() + 1) / 2;
		Mat adaptivHough = adaptivHough(houghTransform, blurSize);
		List<Double> deltaAngle = IntStream.range(0, adaptivHough.cols()).mapToObj(col -> Math.abs(Math.atan((double) (col - stripWidth + 1) / (stripWidth - 1)) - Math.atan((double) (col - stripWidth + 2) / (stripWidth - 1))) * 180 / Math.PI)
				.collect(Collectors.toList());
		double[][] score = new double[adaptivHough.rows()][adaptivHough.cols()];
		int[][] previousCols = new int[adaptivHough.rows()][adaptivHough.cols()];
		for (int col = 0; col < adaptivHough.cols(); col++)
			score[0][col] = adaptivHough.get(0, col)[0];
		for (int row = 1; row < adaptivHough.rows(); row++) {
			double[] adaptivHoughRow = new double[adaptivHough.cols()];
			adaptivHough.row(row).get(0, 0, adaptivHoughRow);

			for (int col = 0; col < adaptivHoughRow.length; col++) {
				double adaptiveMagnitude = adaptivHoughRow[col];

				double previousColScore = col == 0 ? Double.NEGATIVE_INFINITY : score[row - 1][col - 1] + anglePenality * deltaAngle.get(col - 1);
				double sameColScore = score[row - 1][col];
				double nextColScore = col == adaptivHoughRow.length - 1 ? Double.NEGATIVE_INFINITY : score[row - 1][col + 1] + anglePenality * deltaAngle.get(col);

				double bestScore = -1;
				if (sameColScore >= previousColScore && sameColScore >= nextColScore) {
					bestScore = sameColScore;
					previousCols[row][col] = col;
				} else if (previousColScore >= sameColScore && previousColScore >= nextColScore) {
					bestScore = previousColScore;
					previousCols[row][col] = col - 1;
				} else {
					bestScore = nextColScore;
					previousCols[row][col] = col + 1;
				}
				score[row][col] = adaptiveMagnitude + bestScore;
			}
		}
		adaptivHough.release();
		return Arrays.asList(buildResult(score, previousCols, stripWidth, houghTransform));
	}

	private static TrajectStep[] buildResult(double[][] score, int[][] previousCols, int stripWidth, Mat houghTransform) {
		TrajectStep[] result = new TrajectStep[score.length];
		int col = getBestScoreCol(score);
		for (int y = score.length - 1; y >= 0; y--) {
			result[y] = new TrajectStep(y, ((double) col - (stripWidth - 1)) / (stripWidth - 1), houghTransform.get(y, col)[0]);
			col = previousCols[y][col];
		}
		return result;
	}

	private static int getBestScoreCol(double[][] scores) {
		double maxScore = Double.NEGATIVE_INFINITY;
		int maxScoreCol = -1;
		for (int col = 0; col < scores[0].length; col++) {
			double colScore = scores[scores.length - 1][col];
			if (colScore > maxScore) {
				maxScore = colScore;
				maxScoreCol = col;
			}
		}
		return maxScoreCol;
	}

	public static List<TrajectStep> bestTrajectFHT2(Mat houghTransform, int blurSize, double penality, int stepCount) {
		int stripWidth = (houghTransform.cols() + 1) / 2;
		Mat adaptivHough = adaptivHough(houghTransform, blurSize);
		// Core.pow(houghTransform, 3, adaptivHough);
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

	public static List<TrajectStep> bestInfluencedTrajectFHT(Mat houghTransform, int blurSize, double anglePenality, double neigbourPenality, StripTractor[] prevStripInfluences, StripTractor[] nextStripInfluences) {
		int stripWidth = (houghTransform.cols() + 1) / 2;
		Mat adaptivHough = adaptivHough(houghTransform, blurSize);
		List<Double> deltaAngle = IntStream.range(0, adaptivHough.cols()).mapToObj(col -> Math.abs(Math.atan((double) (col - stripWidth + 1) / (stripWidth - 1)) - Math.atan((double) (col - stripWidth + 2) / (stripWidth - 1))) * 180 / Math.PI)
				.collect(Collectors.toList());
		double[][] score = new double[adaptivHough.rows()][adaptivHough.cols()];
		int[][] previousCols = new int[adaptivHough.rows()][adaptivHough.cols()];
		for (int col = 0; col < adaptivHough.cols(); col++)
			score[0][col] = adaptivHough.get(0, col)[0];
		for (int row = 1; row < adaptivHough.rows(); row++) {
			double[] adaptivHoughRow = new double[adaptivHough.cols()];
			adaptivHough.row(row).get(0, 0, adaptivHoughRow);

			for (int col = 0; col < adaptivHoughRow.length; col++) {
				double adaptiveMagnitude = adaptivHoughRow[col];

				double previousColScore = col == 0 ? Double.NEGATIVE_INFINITY : score[row - 1][col - 1] + anglePenality * deltaAngle.get(col - 1);
				double sameColScore = score[row - 1][col];
				double nextColScore = col == adaptivHoughRow.length - 1 ? Double.NEGATIVE_INFINITY : score[row - 1][col + 1] + anglePenality * deltaAngle.get(col);

				double bestScore = -1;
				if (sameColScore >= previousColScore && sameColScore >= nextColScore) {
					bestScore = sameColScore;
					previousCols[row][col] = col;
				} else if (previousColScore >= sameColScore && previousColScore >= nextColScore) {
					bestScore = previousColScore;
					previousCols[row][col] = col - 1;
				} else {
					bestScore = nextColScore;
					previousCols[row][col] = col + 1;
				}
				double derivative = (double) (col - stripWidth + 1) / (stripWidth - 1);
				score[row][col] = adaptiveMagnitude + bestScore + neigbourPenality * prevStripInfluences[row].getInfluence(derivative) + neigbourPenality * (nextStripInfluences[row].getInfluence(derivative));
			}
		}
		adaptivHough.release();
		return Arrays.asList(buildResult(score, previousCols, stripWidth, houghTransform));
	}

	private static TrajectStep[] buildResult2(List<Double> derivativeFromIndex, double[][] score, int[][] thetaPrev, int stripWidth, Mat houghTransform, double step) {
		int prevTheta = getBestScoreCol(score);
		TrajectStep[] result = new TrajectStep[score.length];
		for (int i = score.length - 1; i >= 0; i--) {
			int row = (int) Math.round(i * step);
			double derivative = derivativeFromIndex.get(prevTheta);
			result[i] = new TrajectStep(row, derivative, houghTransform.get(row, prevTheta)[0]);
			prevTheta = thetaPrev[i][prevTheta];
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

	public static void displayHSplines(List<PolynomialSplineFunction> vRadonSplinesFunctions, Mat image, double... color) {
		vRadonSplinesFunctions.forEach(spline -> {
			for (double x = spline.getKnots()[0]; x < spline.getKnots()[spline.getKnots().length - 1]; x++)
				image.put((int) Math.round(spline.value(x)), (int) x, color);
		});
	}

	public static void displayVSplines(List<PolynomialSplineFunction> hRadonSplinesFunctions, Mat image, double... color) {
		hRadonSplinesFunctions.forEach(spline -> {
			for (double y = spline.getKnots()[0]; y < spline.getKnots()[spline.getKnots().length - 1]; y++)
				image.put((int) y, (int) Math.round(spline.value(y)), color);
		});
	}

}
