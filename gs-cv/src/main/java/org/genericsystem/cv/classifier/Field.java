package org.genericsystem.cv.classifier;

import org.genericsystem.cv.Img;
import org.opencv.core.Rect;

public class Field extends AbstractField {

	public Field(Rect rect) {
		super(rect);
	}

	@Override
	public void ocr(Img rootImg) {
		super.ocr(rootImg);
		if (attempts <= 3 || attempts % 5 == 0)
			consolidateOcr();
	}

}