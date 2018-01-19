package org.genericsystem.cv.application;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Calibrated;
import org.genericsystem.cv.Kalman;
import org.genericsystem.cv.Lines;
import org.opencv.core.Mat;

public class Deperspectiver {

	private AngleCalibrated calibrated0 = new AngleCalibrated(0, Math.PI / 2);
	private Kalman kalmanZ = new Kalman();
	private AngleCalibrated[] calibratedVps;


	public Mat doWork(SuperFrameImg superFrame, double f, double[] pp, boolean textsEnabledMode, Lines lines) {
		if (textsEnabledMode)
			lines.getLines().addAll(superFrame.findTextOrientationLines());
		if (lines.size() > 4) {
			calibrated0 = superFrame.findVanishingPoint(lines, calibrated0);
			calibratedVps = superFrame.findOtherVps(calibrated0, lines);

			double[] predictionZ = kalmanZ.predict();
			kalmanZ.correct(calibratedVps[2].uncalibrate(pp, f));
			calibratedVps[2] = new AngleCalibrated(new double[] { predictionZ[0], predictionZ[1], 1.0 }, pp, f);
			calibratedVps[1] = calibratedVps[0].getOrthoFromVps(calibratedVps[2]);

			return superFrame.findHomography(calibratedVps);

		} else {
			System.out.println("Not enough lines : " + lines.size());
			return null;
		}
	}

	public AngleCalibrated[] getCalibratedVps() {
		return calibratedVps;
	}

}
