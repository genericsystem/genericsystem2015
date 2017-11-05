package org.genericsystem.cv;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;

public class Calibrated {
	final double x, y, z;

	static Mat K = new Mat(3, 1, CvType.CV_64FC1);
	static Mat Kinv;

	public static void calibrate(double width, double height) {
		K = new Mat(3, 3, CvType.CV_64FC1, new Scalar(0));
		K.put(0, 0, width);
		K.put(0, 2, width / 2);
		K.put(1, 1, height);
		K.put(1, 2, height);
		K.put(2, 2, 1d);
		Kinv = K.inv();
	}

	public Calibrated(Point uncalibrate) {
		Mat calibrated = calibrate(uncalibrate);
		x = calibrated.get(0, 0)[0];
		y = calibrated.get(1, 0)[0];
		z = calibrated.get(2, 0)[0];
	}

	public Calibrated(double[] calibratedxyz) {
		x = calibratedxyz[0];
		y = calibratedxyz[1];
		z = calibratedxyz[2];
	}

	double[] getCalibratexyz() {
		return new double[] { x, y, z };
	}

	Calibrated dump(double[] xyz, int dumpSize) {
		return new Calibrated(new double[] { ((dumpSize - 1) * x + xyz[0]) / dumpSize, ((dumpSize - 1) * y + xyz[1]) / dumpSize, ((dumpSize - 1) * z + xyz[2]) / dumpSize });
	}

	public static Mat calibrate(Point vp) {
		Mat result = new Mat(3, 1, CvType.CV_64FC1);
		result.put(0, 0, vp.x);
		result.put(1, 0, vp.y);
		result.put(2, 0, 1d);
		Core.gemm(Kinv, result, 1, new Mat(), 0, result);
		Core.normalize(result, result);
		return result;
	}

	public Point uncalibrate() {
		Mat result = new Mat(3, 1, CvType.CV_64FC1);
		result.put(0, 0, x);
		result.put(1, 0, y);
		result.put(2, 0, z);
		Core.gemm(K, result, 1, new Mat(), 0, result);
		// if (result.get(2, 0)[0] != 0) {
		// result.put(0, 0, result.get(0, 0)[0] / result.get(2, 0)[0]);
		// result.put(1, 0, result.get(1, 0)[0] / result.get(2, 0)[0]);
		// result.put(2, 0, 1d);
		// }
		return new Point(result.get(0, 0)[0] / result.get(2, 0)[0], result.get(1, 0)[0] / result.get(2, 0)[0]);
	}

	public static class AngleCalibrated extends Calibrated {

		final double tetha;

		final double phi;

		public AngleCalibrated(Point uncalibrate) {
			super(uncalibrate);
			tetha = Math.acos(z);
			phi = Math.atan2(y, x);
		}

		public AngleCalibrated(double[] tethaPhi) {
			super(new double[] { Math.cos(tethaPhi[1]) * Math.sin(tethaPhi[0]), Math.sin(tethaPhi[1]) * Math.sin(tethaPhi[0]), Math.cos(tethaPhi[0]) });
			this.tetha = tethaPhi[0];
			this.phi = tethaPhi[1];
		}

		public double[] getTethaPhi() {
			return new double[] { tetha, phi };
		}

		@Override
		public AngleCalibrated dump(double[] tethaPhi, int dumpSize) {
			return new AngleCalibrated(new double[] { ((dumpSize - 1) * tetha + tethaPhi[0]) / dumpSize, ((dumpSize - 1) * phi + tethaPhi[1]) / dumpSize });
		}
	}
}