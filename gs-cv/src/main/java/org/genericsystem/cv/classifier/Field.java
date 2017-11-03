package org.genericsystem.cv.classifier;

import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class Field extends AbstractField {

	private static final int LABELS_SIZE_THRESHOLD = 20;
	private static final double CONFIDENCE_THRESHOLD = 0.95;
	private boolean locked = false;

	public Field(Rect rect) {
		super(rect);
	}

	@Override
	public void ocr(Img rootImg) {
		super.ocr(rootImg);
		if (attempts <= 3 || attempts % 5 == 0)
			consolidateOcr();
	}

	@Override
	public void merge(AbstractField field) {
		super.merge(field);
		if (field instanceof Field)
			setFinal();
	}

	public void drawLockedField(Img display, Mat homography) {
		if (locked) {
			List<Point> points = Arrays.asList(new Point(rect.x, rect.y), new Point(rect.x + rect.width - 1, rect.y), new Point(rect.x + rect.width - 1, rect.y + rect.height - 1), new Point(rect.x, rect.y + rect.height - 1));
			MatOfPoint2f results = new MatOfPoint2f();
			Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(points), results, homography);
			Point[] targets = results.toArray();
			for (int i = 0; i < 4; ++i) {
				Imgproc.line(display.getSrc(), targets[i], targets[(i + 1) % 4], new Scalar(255, 255, 0), 1);
			}
		}
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