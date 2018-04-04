package org.genericsystem.cv.application;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class RadonTransform {

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
			Imgproc.warpAffine(dst, rotated, rotation, new Size(dst.cols(), dst.rows()), Imgproc.INTER_NEAREST);
			Core.reduce(rotated, rotated, 1, Core.REDUCE_SUM);
			for (int row = 0; row < rotated.rows(); row++)
				radon_image.put(row, t + 45, rotated.get(row, 0)[0]);
		}
		Core.normalize(radon_image, radon_image, 0, 255, Core.NORM_MINMAX);
		radon_image.convertTo(radon_image, CvType.CV_8UC1);
		return radon_image;
	}

	public static Mat projectionMap(Mat radon) {
		System.out.println(radon);
		Mat projectionMap = Mat.zeros((int) (radon.rows() / Math.sqrt(2)), radon.cols(), CvType.CV_8UC1);
		for (int k = 0; k < projectionMap.rows(); k++) {
			for (int tetha = 0; tetha < projectionMap.cols(); tetha++) {
				// System.out.println((k - projectionMap.rows() / 2));
				// System.out.println(Math.sin(((double) tetha - 45) / 180 * Math.PI));
				System.out.println((k - projectionMap.rows() / 2) * Math.sin(((double) tetha - 45) / 180 * Math.PI));
				int p = (int) ((projectionMap.rows() / 2 - k) * Math.sin(((double) tetha + 45) / 180 * Math.PI) + radon.rows() / 2);
				// System.out.println(k + " " + tetha + " " + p + " " + radon.get(p, tetha)[0]);
				projectionMap.put(k, tetha, radon.get(p, tetha > 90 ? tetha - 90 : tetha)[0]);
			}
		}
		return projectionMap;
	}
}
