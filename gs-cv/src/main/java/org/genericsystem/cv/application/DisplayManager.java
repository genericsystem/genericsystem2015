package org.genericsystem.cv.application;

import java.util.List;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Lines;
import org.genericsystem.cv.ReferenceManager;
import org.genericsystem.cv.SuperFrameImg;
import org.genericsystem.cv.SuperTemplate;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;

import javafx.scene.image.Image;

public class DisplayManager {

	public Image displayFrame(SuperFrameImg superFrame, Lines lines, AngleCalibrated[] calibratedVps) {
		superFrame.draw(lines, new Scalar(0, 0, 255), 1);
		superFrame.drawVanishingPointLines(lines, calibratedVps[0], new Scalar(0, 255, 0), 1);
		superFrame.drawVanishingPointLines(lines, calibratedVps[1], new Scalar(255, 0, 0), 1);
		superFrame.drawVpsArrows(calibratedVps, new double[] { 20, 20 }, new Scalar(0, 255, 0), 2);
		return superFrame.getDisplay().toJfxImage();
	}

	public Image displaySuperDeperspectived(SuperFrameImg superDeperspectived, List<Rect> detectedRects) {
		superDeperspectived.drawRects(detectedRects, new Scalar(255), -1);
		return superDeperspectived.getDisplay().toJfxImage();
	}

	public Image displayDiffFrame(SuperFrameImg superDeperspectived) {
		return superDeperspectived.getDiffFrame().toJfxImage();
	}

	public Image displayReferenceFrame(ReferenceManager referenceManager, double[] pp, double f) {
		SuperTemplate superTemplate = new SuperTemplate(referenceManager.getReference().getSuperFrame().getFrame().getSrc(), pp, f);
		List<Rect> referenceRects = referenceManager.getReferenceRects();
		superTemplate.drawRects(referenceRects, new Scalar(255), -1);
		return superTemplate.getDisplay().toJfxImage();
	}
}