//package org.genericsystem.cv;
//
//import org.opencv.core.Point;
//
//public class AngleCalibrated extends Calibrated {
//
//	final double tetha;
//
//	final double phi;
//
//	public AngleCalibrated(Point uncalibrate) {
//		super(uncalibrate);
//		tetha = Math.acos(z);
//		phi = Math.atan2(y, x);
//	}
//
//	public AngleCalibrated(double[] tethaPhi) {
//		super(new double[] { Math.cos(tethaPhi[1]) * Math.sin(tethaPhi[0]), Math.sin(tethaPhi[1]) * Math.sin(tethaPhi[0]), Math.cos(tethaPhi[1]) });
//		this.tetha = tethaPhi[0];
//		this.phi = tethaPhi[1];
//	}
//
//	double[] getTethaPhi() {
//		return new double[] { tetha, phi };
//	}
//
//	@Override
//	AngleCalibrated dump(double[] tethaPhi, int dumpSize) {
//		return new AngleCalibrated(new double[] { ((dumpSize - 1) * tetha + tethaPhi[0]) / dumpSize, ((dumpSize - 1) * phi + tethaPhi[1]) / dumpSize });
//	}
// }