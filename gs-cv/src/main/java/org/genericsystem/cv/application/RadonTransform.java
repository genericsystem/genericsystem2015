package org.genericsystem.cv.application;

import java.util.Arrays;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RadonTransform {

	private static final Logger logger = LoggerFactory.getLogger(RadonTransform.class);

	static {
		NativeLibraryLoader.load();
	}

	public static Mat transform(Mat src) {
		Mat dst = new Mat(src.rows(), src.rows(), CvType.CV_64FC1, new Scalar(0));
		int center = dst.rows() / 2;
		Mat src64 = new Mat();
		src.convertTo(src64, CvType.CV_64FC1);
		src64.copyTo(new Mat(dst, new Rect(new Point(center - src64.cols() / 2, 0), new Point(center + src64.cols() / 2, src64.rows()))));
		Mat radon_image = new Mat(dst.rows(), 90, CvType.CV_64FC1, new Scalar(0));
		for (int t = -45; t < 45; t++) {
			Mat rotated = new Mat();
			Mat rotation = Imgproc.getRotationMatrix2D(new Point(center, center), t, 1);
			Imgproc.warpAffine(dst, rotated, rotation, new Size(dst.cols(), dst.rows()), Imgproc.INTER_LINEAR);
			Core.reduce(rotated, rotated, 1, Core.REDUCE_SUM);
			for (int row = 0; row < rotated.rows(); row++)
				radon_image.put(row, t + 45, rotated.get(row, 0)[0]);
		}
		Core.normalize(radon_image, radon_image, 0, 255, Core.NORM_MINMAX);
		// radon_image.convertTo(radon_image, CvType.CV_8UC1);
		return radon_image;
	}

	public static Mat projectionMap(Mat radon) {
		Mat projectionMap = Mat.zeros(radon.rows(), radon.cols(), CvType.CV_64FC1);
		for (int k = 0; k < projectionMap.rows(); k++) {
			for (int tetha = 0; tetha < projectionMap.cols(); tetha++) {
				int p = (int) ((k - projectionMap.rows() / 2) * Math.sin(((double) tetha + 45) / 180 * Math.PI) + radon.rows() / 2);
				projectionMap.put(k, tetha, radon.get(p, tetha)[0]);
			}
		}
		return projectionMap;
	}

	public static void main(String[] args) {
		Mat projectionMat = Mat.eye(new Size(3, 3), CvType.CV_64FC1);
		System.out.println(Arrays.toString(bestTraject(projectionMat, 0)));
	}

	public static int[] bestTraject(Mat projectionMap, double anglePenality) {
		double[][] score = new double[projectionMap.rows()][projectionMap.cols()];
		int[][] thetaPrev = new int[projectionMap.rows()][projectionMap.cols()];
		for (int theta = 0; theta < projectionMap.cols(); theta++)
			score[0][theta] = Math.pow(projectionMap.get(0, theta)[0], 3);
		for (int k = 1; k < projectionMap.rows(); k++) {
			for (int theta = 0; theta < projectionMap.cols(); theta++) {
				double magnitude = projectionMap.get(k, theta)[0];

				double scoreFromPrevTheta = theta != 0 ? score[k - 1][theta - 1] : Double.NEGATIVE_INFINITY;
				double scoreFromSameTheta = score[k - 1][theta];
				double scoreFromNextTheta = theta < projectionMap.cols() - 1 ? score[k - 1][theta + 1] : Double.NEGATIVE_INFINITY;

				double bestScore4Pos = -1;

				if (scoreFromSameTheta >= (scoreFromPrevTheta + anglePenality) && scoreFromSameTheta >= (scoreFromNextTheta + anglePenality)) {
					bestScore4Pos = scoreFromSameTheta;
					thetaPrev[k][theta] = theta;
				} else if ((scoreFromPrevTheta + anglePenality) >= scoreFromSameTheta && ((scoreFromPrevTheta + anglePenality) >= (scoreFromNextTheta + anglePenality))) {
					bestScore4Pos = scoreFromPrevTheta + anglePenality;
					thetaPrev[k][theta] = theta - 1;
				} else {
					bestScore4Pos = scoreFromNextTheta + anglePenality;
					thetaPrev[k][theta] = theta + 1;
				}
				score[k][theta] = Math.pow(magnitude, 3) + bestScore4Pos;
			}
		}

		// System.out.println(Arrays.toString(score[projectionMap.rows() - 1]));
		// System.out.println(Arrays.deepToString(thetaPrev));
		double maxScore = Double.NEGATIVE_INFINITY;
		int prevTheta = -1;
		int[] thetas = new int[projectionMap.rows()];
		for (int theta = 0; theta < projectionMap.cols(); theta++) {
			double lastScore = score[projectionMap.rows() - 1][theta];
			// System.out.println(lastScore);
			if (lastScore > maxScore) {
				maxScore = lastScore;
				prevTheta = theta;
			}
		}
		assert prevTheta != -1;

		// System.out.println(maxScore + " for theta : " + prevTheta);
		for (int k = projectionMap.rows() - 1; k >= 0; k--) {
			thetas[k] = prevTheta;
			// System.out.println(prevTheta);
			prevTheta = thetaPrev[k][prevTheta];
		}

		return thetas;
	}

	public static Mat extractStrip(Mat src, int startX, int width) {
		return new Mat(src, new Range(0, src.rows()), new Range(startX, startX + width));
	}

	public static Mat radonTransform(Mat strip) {
		Mat dst = new Mat(strip.rows(), strip.rows(), CvType.CV_64FC1, new Scalar(0));
		Mat src64 = new Mat();
		strip.convertTo(src64, CvType.CV_64FC1);
		int xStart = strip.rows() / 2 - strip.width() / 2;
		for (int i = 0; i < strip.width(); i++)
			for (int j = 0; j < strip.height(); j++)
				dst.put(j, xStart + i, src64.get(j, i)[0]);

		Mat radon_image = new Mat(strip.rows(), 90, CvType.CV_64FC1, new Scalar(0));
		for (int t = -45; t < 45; t++) {
			Mat rotated = new Mat();
			Mat rotation = Imgproc.getRotationMatrix2D(new Point(strip.width() / 2, strip.height() / 2), t, 1);
			Imgproc.warpAffine(dst, rotated, rotation, dst.size(), Imgproc.INTER_NEAREST);
			Core.reduce(rotated, rotated, 1, Core.REDUCE_SUM);
			for (int row = 0; row < rotated.rows(); row++)
				radon_image.put(row, t + 45, rotated.get(row, 0)[0]);
		}
		Core.normalize(radon_image, radon_image, 0, 255, Core.NORM_MINMAX);
		return radon_image;
	}

	public static Mat estimateBaselines(Mat image, double anglePenalty) {
		Mat result = image.clone();
		// Number of overlapping vertical strips.
		int n = 20;
		// Overlap ratio between two consecutive strips.
		float r = .5f;
		// w = width of a vertical strip.
		// Image width = [n(1 - r) + r] w
		int w = (int) (image.width() / (n * (1 - r) + r));
		int step = (int) ((1 - r) * w);
		int[][] angles = new int[n][];
		int x = 0;
		for (int i = 0; i < n; i++) {
			Mat radonTransform = radonTransform(extractStrip(image, x, w));
			Mat projMap = projectionMap(radonTransform);
			angles[i] = bestTraject(projMap, anglePenalty);
			x += step;
		}

		int lines = 10;
		int yStep = image.height() / (lines + 1);
		double[] xs = new double[n];
		for (int j = 0; j < n; j++)
			xs[j] = (j + .5) * step;

		for (int i = 0; i < lines; i++) {
			int currY = i * yStep + (int)(.5 * yStep);
			double[] ys = new double[n];
			logger.info("Line {}", i);
			for (int j = 0; j < n; j++) {
				ys[j] = currY;
				double theta = (double) (135 - angles[j][currY]) / 180 * Math.PI;
				// Line passing by the point G at the middle of the strip with ordinate currY (x_G, y_G),
				// whose normal makes an angle of theta with the horizontal:
				// y = y_G - (x - x_G) cotan theta
				// Ordinate of the next point:
				currY -= (int) (step * 1 / Math.tan(theta));

				if (currY < 0 || currY > image.height())
					break;
				else if (j > 0) {
					System.out.printf("theta: %3d, (%3.1f, %3.1f) ", 135 - angles[j][currY], xs[j - 1], ys[j - 1]);
					Imgproc.line(result, new Point(xs[j - 1], ys[j - 1]), new Point(xs[j], ys[j]), new Scalar(255, 0, 0));
				}
				System.out.println();
			}
			// TODO: Approximate line by a curve
		}

		for (int j = 0; j < image.rows(); j += 10) {
			for (int i = 0; i < n; i++)
				System.out.printf("%3d ", 135 - angles[i][j]);
			System.out.println();
		}

		return result;
	}
}
