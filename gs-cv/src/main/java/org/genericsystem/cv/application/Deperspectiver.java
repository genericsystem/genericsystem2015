package org.genericsystem.cv.application;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Kalman;
import org.genericsystem.cv.Lines;
import org.opencv.core.Mat;

public class Deperspectiver {

	private final double f;
	private final double[] pp;
	private AngleCalibrated calibrated0 = new AngleCalibrated(0, Math.PI / 2);
	private Kalman kalmanZ = new Kalman();
	private AngleCalibrated[] calibratedVps;

	public Deperspectiver(double f, double[] pp) {
		this.f = f;
		this.pp = pp;
	}

	public AngleCalibrated[] computeCalibratedVps(SuperFrameImg superFrame, boolean textsEnabledMode, Lines lines) {
		if (textsEnabledMode)
			lines.getLines().addAll(superFrame.findTextOrientationLines());
		if (lines.size() > 4) {
			calibrated0 = superFrame.findVanishingPoint(lines, calibrated0);
			calibratedVps = superFrame.findOtherVps(calibrated0, lines);

			double[] predictionZ = kalmanZ.predict();
			kalmanZ.correct(calibratedVps[2].uncalibrate(pp, f));
			calibratedVps[2] = new AngleCalibrated(new double[] { predictionZ[0], predictionZ[1], 1.0 }, pp, f);
			calibratedVps[1] = calibratedVps[0].getOrthoFromVps(calibratedVps[2]);
			return calibratedVps;

		} else {
			System.out.println("Not enough lines : " + lines.size());
			return null;
		}
	}

	public Mat findHomography(SuperFrameImg superFrame, AngleCalibrated[] calibratedVps) {
		return superFrame.findHomography(calibratedVps);
	}

}
