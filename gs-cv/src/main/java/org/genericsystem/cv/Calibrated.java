package org.genericsystem.cv;

import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.Deperspectiver.Line;
import org.genericsystem.cv.Deperspectiver.Lines;

public class Calibrated {

	final double x, y, z;

	public Calibrated(double[] vp, double[] pp, double f) {
		double[] calibratedxyz = calibrate(vp, pp, f);
		if (calibratedxyz[2] < 0) {
			calibratedxyz[0] = -calibratedxyz[0];
			calibratedxyz[1] = -calibratedxyz[1];
			calibratedxyz[2] = -calibratedxyz[2];
		}
		x = calibratedxyz[0];
		y = calibratedxyz[1];
		z = calibratedxyz[2];

	}

	public Calibrated(double[] calibratedxyz) {
		if (calibratedxyz[2] < 0) {
			calibratedxyz[0] = -calibratedxyz[0];
			calibratedxyz[1] = -calibratedxyz[1];
			calibratedxyz[2] = -calibratedxyz[2];
		}
		x = calibratedxyz[0];
		y = calibratedxyz[1];
		z = calibratedxyz[2];
	}

	public double[] getCalibratexyz() {
		return new double[] { x, y, z };
	}

	Calibrated dumpXyz(double[] xyz, int dumpSize) {
		return new Calibrated(new double[] { ((dumpSize - 1) * x + xyz[0]) / dumpSize, ((dumpSize - 1) * y + xyz[1]) / dumpSize, ((dumpSize - 1) * z + xyz[2]) / dumpSize });
	}

	public static double[] calibrate(double[] vpImg, double[] pp, double f) {
		double[] vp = new double[] { vpImg[0] / vpImg[2] - pp[0], vpImg[1] / vpImg[2] - pp[1], f };
		if (vp[2] == 0)
			vp[2] = 0.0011;
		double N = Math.sqrt(vp[0] * vp[0] + vp[1] * vp[1] + vp[2] * vp[2]);
		vp[0] *= 1.0 / N;
		vp[1] *= 1.0 / N;
		vp[2] *= 1.0 / N;
		if (vp[2] < 0) {
			vp[0] = -vp[0];
			vp[1] = -vp[1];
			vp[2] = -vp[2];
		}
		return vp;

	}

	public double[] uncalibrate(double[] pp, double f) {
		double[] result = new double[3];
		result[0] = x * f / z + pp[0];
		result[1] = y * f / z + pp[1];
		result[2] = 1.0;
		return result;
	}

	public static class AngleCalibrated extends Calibrated {

		final double theta;
		final double phi;

		public AngleCalibrated(double[] vp, double[] pp, double f) {
			super(vp, pp, f);
			theta = Math.atan2(this.y, this.x);
			phi = Math.acos(this.z);
		}

		public AngleCalibrated(double[] calibratedxyz, Object nullObject) {
			super(calibratedxyz);
			this.theta = Math.atan2(this.y, this.x);
			this.phi = Math.acos(this.z);
		}

		public AngleCalibrated(double theta, double phi) {
			this(new double[] { theta, phi });
		}

		public AngleCalibrated(double[] tethaPhi) {
			super(new double[] { Math.sin(tethaPhi[1]) * Math.cos(tethaPhi[0]), Math.sin(tethaPhi[1]) * Math.sin(tethaPhi[0]), Math.cos(tethaPhi[1]) });
			this.theta = Math.atan2(this.y, this.x);
			this.phi = Math.acos(this.z);
		}

		public double getTheta() {
			return theta;
		}

		public double getPhi() {
			return phi;
		}

		public double[] getThetaPhi() {
			return new double[] { theta, phi };
		}

		public AngleCalibrated dumpThetaPhi(double[] tethaPhi, int dumpSize) {
			return new AngleCalibrated(new double[] { ((dumpSize - 1) * theta + tethaPhi[0]) / dumpSize, ((dumpSize - 1) * phi + tethaPhi[1]) / dumpSize });
		}

		@Override
		AngleCalibrated dumpXyz(double[] xyz, int dumpSize) {
			return new AngleCalibrated(new double[] { ((dumpSize - 1) * x + xyz[0]) / dumpSize, ((dumpSize - 1) * y + xyz[1]) / dumpSize, ((dumpSize - 1) * z + xyz[2]) / dumpSize }, null);
		}

		public AngleCalibrated getOrthoFromAngle(double lambda) {
			double[] vp1 = getCalibratexyz();
			double k1 = vp1[0] * Math.sin(lambda) + vp1[1] * Math.cos(lambda);
			double k2 = vp1[2];
			double phi = Math.atan(-k2 / k1);
			double[] result = new double[] { Math.sin(phi) * Math.sin(lambda), Math.sin(phi) * Math.cos(lambda), Math.cos(phi) };
			if (result[2] == 0.0)
				result[2] = 0.0011;
			double N = Math.sqrt(result[0] * result[0] + result[1] * result[1] + result[2] * result[2]);
			result[0] *= 1.0 / N;
			result[1] *= 1.0 / N;
			result[2] *= 1.0 / N;
			// if (result[2] < 0) {
			// result[0] *= -1.0;
			// result[1] *= -1.0;
			// result[2] *= -1.0;
			// }
			return new AngleCalibrated(result, null);
		}

		public AngleCalibrated getOrthoFromVps(Calibrated calibrated2) {
			double[] vp1 = getCalibratexyz();
			double[] vp2 = calibrated2.getCalibratexyz();
			double[] vp3 = cross(vp1, vp2);
			if (vp3[2] == 0.0)
				vp3[2] = 0.0011;
			double N = Math.sqrt(vp3[0] * vp3[0] + vp3[1] * vp3[1] + vp3[2] * vp3[2]);
			vp3[0] *= 1.0 / N;
			vp3[1] *= 1.0 / N;
			vp3[2] *= 1.0 / N;
			// if (vp3[2] < 0) {
			// vp3[0] *= -1.0;
			// vp3[1] *= -1.0;
			// vp3[2] *= -1.0;
			// }
			return new AngleCalibrated(vp3, null);
		}

		static double[] cross(double[] a, double b[]) {
			return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
		}

		@Override
		public String toString() {
			return "(" + theta * 180 / Math.PI + "°, " + phi * 180 / Math.PI + "°)";
		}

		public double distance(List<Line> lines, double[] pp, double f) {
			double[] uncalibrate = uncalibrate(pp, f);
			double error = 0;
			for (Line line : lines)
				error += Math.pow(Math.min(AngleCalibrated.distance(uncalibrate, line), 0.3) * line.size(), 2);
			return error;
		}

		public double distance(Line line, double[] pp, double f) {
			return Math.pow(Math.min(AngleCalibrated.distance(uncalibrate(pp, f), line), 0.3) * line.size(), 2);
		}

		static double distance(double[] vp, Line line) {
			double dy = line.y1 - line.y2;
			double dx = line.x2 - line.x1;
			double dz = line.y1 * line.x2 - line.x1 * line.y2;
			double norm = Math.sqrt(dy * dy + dx * dx + dz * dz);
			double n0 = -dx / norm;
			double n1 = dy / norm;
			double nNorm = Math.sqrt(n0 * n0 + n1 * n1);
			double[] midPoint = new double[] { (line.x1 + line.x2) / 2, (line.y1 + line.y2) / 2, 1d };
			double r0 = vp[1] * midPoint[2] - midPoint[1];
			double r1 = midPoint[0] - vp[0] * midPoint[2];
			double rNorm = Math.sqrt(r0 * r0 + r1 * r1);
			double num = r0 * n0 + r1 * n1;
			if (num < 0)
				num = -num;
			double d = 0;
			if (nNorm != 0 && rNorm != 0)
				d = num / (nNorm * rNorm);
			return d;
		}

		public AngleCalibrated[] findOtherVps(Lines lines, double[] pp, double f) {
			System.out.println(Arrays.toString(getCalibratexyz()));
			AngleCalibrated[] result = new AngleCalibrated[] { null, null, null };
			double bestError = Double.MAX_VALUE;
			// double bestAngle = 0;
			for (double angle = 0; angle < 360 / 180 * Math.PI; angle += 1 * Math.PI / 180) {
				AngleCalibrated calibratexy = getOrthoFromAngle(angle);
				AngleCalibrated calibratez = getOrthoFromVps(calibratexy);
				if (calibratexy.getPhi() < calibratez.getPhi()) {
					AngleCalibrated tmp = calibratexy;
					calibratexy = calibratez;
					calibratez = tmp;
				}
				double error = calibratexy.distance(lines.lines, pp, f);
				if (error < bestError) {
					bestError = error;
					result[0] = this;
					result[1] = calibratexy;
					result[2] = calibratez;
					// bestAngle = angle;
				}
			}
			double theta0 = Math.abs(result[0].getTheta()) % Math.PI;
			theta0 = Math.min(Math.PI - theta0, theta0);
			double theta1 = Math.abs(result[1].getTheta()) % Math.PI;
			theta1 = Math.min(Math.PI - theta1, theta1);
			if (theta0 > theta1) {
				AngleCalibrated tmp = result[0];
				result[0] = result[1];
				result[1] = tmp;
			}
			return result;
		}
	}
}