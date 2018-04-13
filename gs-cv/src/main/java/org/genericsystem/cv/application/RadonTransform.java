package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

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
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RadonTransform {

	static {
		NativeLibraryLoader.load();
	}

	private static final Logger logger = LoggerFactory.getLogger(RadonTransform.class);

	public static Mat transform(Mat src, int minMaxAngle) {
		Mat dst = Mat.zeros(src.rows(), src.rows(), CvType.CV_64FC1);
		int center = dst.rows() / 2;
		src.convertTo(new Mat(dst, new Rect(new Point(center - src.cols() / 2, 0), new Point(center + src.cols() / 2, src.rows()))), CvType.CV_64FC1);
		Mat radon = Mat.zeros(dst.rows(), 2 * minMaxAngle, CvType.CV_64FC1);
		for (int t = -minMaxAngle; t < minMaxAngle; t++) {
			Mat rotated = new Mat();
			Mat rotation = Imgproc.getRotationMatrix2D(new Point(center, center), t, 1);
			Imgproc.warpAffine(dst, rotated, rotation, new Size(dst.cols(), dst.rows()), Imgproc.INTER_NEAREST);
			Core.reduce(rotated, radon.col(t + minMaxAngle), 1, Core.REDUCE_SUM);
			rotated.release();
			rotation.release();
		}
		dst.release();
		Core.normalize(radon, radon, 0, 255, Core.NORM_MINMAX);
		return radon;
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

	public static int[] bestTraject(Mat projectionMap, double anglePenality, double pow) {
		double[][] score = new double[projectionMap.rows()][projectionMap.cols()];
		int[][] thetaPrev = new int[projectionMap.rows()][projectionMap.cols()];
		for (int theta = 0; theta < projectionMap.cols(); theta++)
			score[0][theta] = Math.pow(projectionMap.get(0, theta)[0], pow);
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
				score[k][theta] = Math.pow(magnitude, pow) + bestScore4Pos;
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

	public static List<Mat> extractStrips(Mat src, int stripWidth) {
		List<Mat> strips = new ArrayList<>();
		for (int col = 0; col + stripWidth <= src.cols(); col += stripWidth / 2)
			strips.add(extractStrip(src, col, stripWidth));
		return strips;
	}

	public static Mat extractStrip(Mat src, int startX, int width) {
		return new Mat(src, new Range(0, src.rows()), new Range(startX, startX + width));
	}

	public static List<PolynomialSplineFunction> estimateBaselines(Mat image, double anglePenalty, int minMaxAngle, double magnitudePow, int yStep) {
		Mat preprocessed = new Img(image, false).adaptativeGaussianInvThreshold(5, 3).getSrc();
		List<PolynomialSplineFunction> hLines = new ArrayList<>();
		// Number of overlapping vertical strips.
		int n = 20;
		// Overlap ratio between two consecutive strips.
		float r = .5f;
		// w = width of a vertical strip.
		// Image width = [n(1 - r) + r] w
		double w = (image.width() / (n * (1 - r) + r));
		double step = (int) ((1 - r) * w);
		int[][] angles = new int[n][];

		// 0, center of each vertical strip, image.width() - 1
		double[] xs = new double[n + 2];

		BiFunction<Double, double[], Double> f = (y, params) -> params[0] + params[1] * y + params[2] * y * y;
		double[][] approxParams = new double[n][];
		int x = 0;
		for (int i = 0; i < n; i++) {
			Mat radonTransform = transform(extractStrip(preprocessed, x, (int) w), minMaxAngle);
			Mat projMap = projectionMap(radonTransform);
			Imgproc.morphologyEx(projMap, projMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 4)));
			angles[i] = bestTraject(projMap, anglePenalty, magnitudePow);
			projMap.release();
			radonTransform.release();

			List<double[]> values = new ArrayList<>();
			for (int k = 0; k < image.height(); k++)
				values.add(new double[] { k, angles[i][k] });
			approxParams[i] = LevenbergImpl.fromBiFunction(f, values, new double[] { 0, 0, 0 }).getParams();
			xs[i + 1] = x + .5 * w;
			x += step;
		}
		xs[n + 1] = image.width() - 1;

		int lines = (image.height() - 1) / yStep + 1;

		logger.info("Image width {}, xs {}, step {}, w {}", image.width(), Arrays.toString(xs), step, w);

		for (int i = 0; i < lines; i++) {
			double[] ys = new double[n + 2];
			// Start building line from the middle.
			ys[n / 2] = i * yStep + .5 * yStep;
			for (int j = n / 2; j <= n; j++) {
				double theta = (f.apply(ys[j], approxParams[j - 1]) - minMaxAngle) / 180 * Math.PI;
				// Line passing by the point G at the middle of the strip with ordinate currY (x_G, y_G),
				// making an angle of theta with the horizontal:
				// y = y_G + (x - x_G) tan theta
				if (j == n)
					ys[n + 1] = ys[n] + (image.width() - 1 - xs[j]) * Math.tan(theta);
				else {
					// Ordinate of the next point:
					ys[j + 1] = ys[j] + step * Math.tan(theta);
				}
			}
			for (int j = n / 2; j > 0; j--) {
				double theta = (f.apply(ys[j], approxParams[j - 1]) - minMaxAngle) / 180 * Math.PI;
				ys[j - 1] = ys[j] - step * Math.tan(theta);
			}

			// Approximate line with polynomial curve.
			PolynomialSplineFunction psf = new LinearInterpolator().interpolate(xs, ys);
			hLines.add(psf);
		}

		return hLines;
	}

	public static Function<Double, Double> approxTraject(int[] traj) {
		List<double[]> values = new ArrayList<>();
		for (int k = 0; k < traj.length; k++)
			values.add(new double[] { k, traj[k] });
		BiFunction<Double, double[], Double> f = (x, params) -> params[0] + params[1] * x + params[2] * x * x + params[3] * x * x * x;
		double[] params = LevenbergImpl.fromBiFunction(f, values, new double[] { 0, 0, 0, 0 }).getParams();
		return x -> f.apply(x, params);
	}

	public static List<OrientedPoint> toHorizontalOrientedPoints(Function<Double, Double> f, int vStrip, int stripWidth, int height, int step) {
		List<OrientedPoint> orientedPoints = new ArrayList<>();
		for (int k = step; k < height; k += step) {
			double angle = (f.apply((double) k) - 45) / 180 * Math.PI;
			orientedPoints.add(new OrientedPoint(new Point((vStrip + 1) * stripWidth / 2, k), angle, 1));
		}
		return orientedPoints;
	}

	public static List<OrientedPoint> toVerticalOrientedPoints(Function<Double, Double> f, int hStrip, int stripHeight, int width, int step) {
		List<OrientedPoint> orientedPoints = new ArrayList<>();
		for (int k = step; k < width; k += step) {
			double angle = (90 + 45 - f.apply((double) k)) / 180 * Math.PI;
			orientedPoints.add(new OrientedPoint(new Point(k, (hStrip + 1) * stripHeight / 2), angle, 1));
		}
		return orientedPoints;
	}

	private static boolean inImage(Point p, Mat img) {
		return p.x >= 0 && p.y >= 0 && p.x < img.width() && p.y < img.height();
	}
}
