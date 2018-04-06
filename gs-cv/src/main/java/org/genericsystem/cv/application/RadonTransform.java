package org.genericsystem.cv.application;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import java.util.Arrays;

public class RadonTransform {

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
				score[k][theta] = magnitude + bestScore4Pos;
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
			assert prevTheta != -1 : k + " " + prevTheta;
		}

		return thetas;
	}
}
