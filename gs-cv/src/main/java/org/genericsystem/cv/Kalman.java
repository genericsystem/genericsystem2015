package org.genericsystem.cv;

import java.util.Arrays;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.video.KalmanFilter;

public class Kalman {

	KalmanFilter kf = new KalmanFilter(4, 2, 1, CvType.CV_64F);

	static {
		NativeLibraryLoader.load();
	}

	public Kalman() {
		Mat transitionMatrix = new Mat(4, 4, CvType.CV_64F, new Scalar(0));
		double[] tM = { 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1 };
		transitionMatrix.put(0, 0, tM);
		kf.set_transitionMatrix(transitionMatrix);

		Mat mm = Mat.eye(2, 4, CvType.CV_64F);
		kf.set_measurementMatrix(mm);

		Mat processNoiseCov = Mat.eye(4, 4, CvType.CV_64F);
		processNoiseCov = processNoiseCov.mul(processNoiseCov, 0.0001);
		kf.set_processNoiseCov(processNoiseCov);

		Mat id1 = Mat.eye(2, 2, CvType.CV_64F);
		id1 = id1.mul(id1, 0.1);
		kf.set_measurementNoiseCov(id1);

		Mat id2 = Mat.eye(4, 4, CvType.CV_64F);
		// id2 = id2.mul(id2,0.1);
		kf.set_errorCovPost(id2);

	}

	public static void main(String[] args) {
		double[][] samplesArr = { { 0, 0 }, { 1, -1 }, { 2, -2 }, { 3, -3 }, { 4, -4 }, { 4, -5 }, { 4, -6 }, { 4, 0 - 7 }, { 4, -8 } };

		Kalman kalman = new Kalman();

		// you'll want to set other Mat's like errorCovPost and processNoiseCov, too, to change the 'adaption speed'

		for (int i = 0; i < samplesArr.length; i++) {
			double[] prediction = kalman.predict();
			System.out.println("Prédiction : " + Arrays.toString(prediction));
			System.out.println("Correction : " + Arrays.toString(samplesArr[i]));
			kalman.correct(samplesArr[i]);
		}

	}

	public double[] predict() {
		Mat prediction = kf.predict();
		return new double[] { prediction.get(0, 0)[0], prediction.get(1, 0)[0] };
	}

	public void correct(double[] correction) {
		Mat meas = new Mat(2, 1, CvType.CV_64F, new Scalar(correction[0], correction[1]));
		meas.put(0, 0, correction);
		kf.correct(meas);
	}

}
