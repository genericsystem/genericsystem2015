package org.genericsystem.cv;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.video.KalmanFilter;

public class Kalman {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		double[][] samplesArr = { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 }, { 4, 0 }, { 4, 0 }, { 4, 0 }, { 4, 0 }, { 4, 0 } };

		Mat meas = new Mat(2, 1, CvType.CV_32F);

		KalmanFilter kf = new KalmanFilter(4, 2);
		Mat transitionMatrix = new Mat(4, 4, CvType.CV_32F, new Scalar(0));
		float[] tM = { 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1 };
		transitionMatrix.put(0, 0, tM);
		kf.set_transitionMatrix(transitionMatrix);
		Mat mm = Mat.eye(2, 4, CvType.CV_32F);
		kf.set_measurementMatrix(mm);

		// you'll want to set other Mat's like errorCovPost and processNoiseCov, too,
		// to change the 'adaption speed'

		for (int i = 0; i < samplesArr.length; i++) {
			meas.put(0, 0, samplesArr[i]);
			System.out.println("----------measures----------------");
			System.out.println(meas.t().dump());

			System.out.println("----------prediction----------------");
			Mat pre = kf.predict();
			System.out.println(pre.t().dump());

			System.out.println("----------correction----------------");
			Mat corr = kf.correct(meas);
			System.out.println(corr.t().dump());
		}

	}

}
