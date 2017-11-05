package org.genericsystem.cv.retriever;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;

public class Field extends AbstractField {

	private static final int LABELS_SIZE_THRESHOLD = 15;
	private static final double CONFIDENCE_THRESHOLD = 0.92;
	private boolean locked = false;

	public Field(Rect rect) {
		super(rect);
	}

	@Override
	public void ocr(Img rootImg) {
		super.ocr(rootImg);
		if (attempts <= 3 || attempts % 5 == 0)
			consolidateOcr(false);
	}

	@Override
	public void merge(AbstractField field) {
		super.merge(field);
		if (field instanceof Field)
			setFinal();
	}

	public void drawLockedField(Img display, Mat homography) {
		if (locked)
			drawRect(display, getRectPointsWithHomography(homography), new Scalar(255, 172, 0), 2);
	}

	public void setFinal() {
		if (!locked)
			if (getLabelsSize() > LABELS_SIZE_THRESHOLD && getConfidence() > CONFIDENCE_THRESHOLD)
				this.locked = true;
	}

	public boolean isLocked() {
		return locked;
	}
}