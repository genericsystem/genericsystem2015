package org.genericsystem.cv;

public class Calibrated {


	final double x, y, z;

	public Calibrated (double[] vp, double[] pp, double f){
		double [] vps = calibrate(vp, pp ,f);
		x = vps[0];
		y = vps[1];
		z = vps[2];	
	}

	public Calibrated(double[] calibratedxyz) {
		x = calibratedxyz[0];
		y = calibratedxyz[1];
		z = calibratedxyz[2];
	}

	public double[] getCalibratexyz() {
		return new double[] { x, y, z };
	}

	Calibrated dump(double[] xyz, int dumpSize) {
		return new Calibrated(new double[] { ((dumpSize - 1) * x + xyz[0]) / dumpSize, ((dumpSize - 1) * y + xyz[1]) / dumpSize, ((dumpSize - 1) * z + xyz[2]) / dumpSize });
	}

	public static double[] calibrate(double[] vpImg, double[] pp, double f){
		double[] vp = new double[] { vpImg[0] / vpImg[2] - pp[0], vpImg[1] / vpImg[2] - pp[1], f };
		if (vp[2] == 0)
			vp[2] = 0.0011;
		double N = Math.sqrt(vp[0] * vp[0] + vp[1] * vp[1] + vp[2] * vp[2]);
		vp[0] *= 1.0 / N;
		vp[1] *= 1.0 / N;
		vp[2] *= 1.0 / N;
		return vp;

	}

	public double[] uncalibrate(double[] pp, double f){
		double[] result = new double[3];
		result[0] = x * f / z + pp[0];
		result[1] = y * f / z + pp[1];
		result[2] = 1.0;
		return result;
	}

	public static class AngleCalibrated extends Calibrated {

		final double tetha;

		final double phi;

		public AngleCalibrated(double[] vp, double [] pp, double f) {
			super(vp, pp, f);
			tetha = Math.acos(z);
			phi = Math.atan2(y, x);
		}

		public AngleCalibrated(double[] calibratedxyz, Object nullObject) {
			super(calibratedxyz);
			this.tetha = Math.atan2(calibratedxyz[1], calibratedxyz[0]);
			this.phi = Math.acos(calibratedxyz[2]);
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

		public double[] getUncalibrated(double[] pp, double f) {
			double[] uncalibrates = new double[3];
			uncalibrates[0] = x * f / z + pp[0];
			uncalibrates[1] = y * f / z + pp[1];
			uncalibrates[2] = 1.0;
			return uncalibrates;
		}

	}
}