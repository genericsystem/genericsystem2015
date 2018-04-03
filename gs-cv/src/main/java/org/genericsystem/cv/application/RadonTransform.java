package org.genericsystem.cv.application;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class RadonTransform {

	public static Mat transform(Mat src) {
		Mat dst = new Mat();
		src.convertTo(dst, CvType.CV_64FC1);
		int angle = 360;
		Mat radon_image = new Mat(dst.rows(), angle, CvType.CV_64FC1, new Scalar(0));
		int center = dst.rows() / 2;
		Mat m0 = Mat.eye(3, 3, CvType.CV_64FC1);
		m0.put(0, 2, -center);
		m0.put(1, 2, -center);
		Mat m1 = Mat.eye(3, 3, CvType.CV_64FC1);
		m1.put(0, 2, center);
		m1.put(1, 2, center);

		for (int t = 0; t < 45; t++) {
			double theta = (t) * Math.PI / angle;
			Mat mR = new Mat(3, 3, CvType.CV_64FC1);
			mR.put(0, 0, Math.cos(theta));
			mR.put(0, 1, Math.sin(theta));
			mR.put(1, 0, -Math.sin(theta));
			mR.put(1, 1, Math.cos(theta));
			mR.put(2, 2, 1);
			Mat tmp = new Mat();
			Core.gemm(m1, mR, 1, new Mat(), 0, tmp);// m1*mR*m0;
			Mat rotation = new Mat();
			Core.gemm(tmp, m0, 1, new Mat(), 0, rotation);// m1*mR*m0;
			Mat rotated = new Mat();
			Imgproc.warpPerspective(dst, rotated, rotation, new Size(dst.rows(), dst.cols()), Imgproc.WARP_INVERSE_MAP);
			for (int i = 0; i < rotated.rows(); i++)
				for (int j = 0; j < rotated.cols(); j++)
					radon_image.put(j, t, radon_image.get(j, t)[0] + rotated.get(i, j)[0]);
		}
		// Core.normalize(radon_image, radon_image, 0, 1, Core.NORM_MINMAX);
		// radon_image.convertTo(radon_image, CvType.CV_8UC1);
		return radon_image;
	}
}
