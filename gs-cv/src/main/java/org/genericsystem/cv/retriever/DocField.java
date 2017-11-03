package org.genericsystem.cv.retriever;

import java.util.Map;
import java.util.Optional;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ModelTools;
import org.opencv.core.Core;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class DocField extends AbstractField {
	private int num;
	private String uid;

	public DocField() {
		super();
	}

	public DocField(int num, Rect rect) {
		super(rect);
		this.num = num;
		this.uid = ModelTools.generateZoneUID(rect);
	}

	public void writeNum(Img img, String text, double fontScale, Scalar color, int thickness) {
		Imgproc.putText(img.getSrc(), text, new Point(rect.tl().x, rect.br().y), Core.FONT_HERSHEY_PLAIN, fontScale, color, thickness);
	}

	public void annotateImage(Img annotated, double fontScale, Scalar color, int thickness) {
		drawRect(annotated, color, thickness);
		writeNum(annotated, String.valueOf(num), fontScale, color, thickness);
	}

	// Getters

	public int getNum() {
		return num;
	}

	public String getUid() {
		return uid;
	}

	// The private setters are needed by Jackson to serialize/de-serialize the JSON objects

	protected void setRect(Rect rect) {
		updateRect(rect);
		this.uid = ModelTools.generateZoneUID(rect);
	}

	protected void setNum(int num) {
		this.num = num;
	}

	protected void setUid(String uid) {
		this.uid = uid;
	}

	protected void setLabels(Map<String, Integer> labels) {
		this.labels = labels;
	}

	protected void setConsolidated(Optional<String> consolidated) {
		this.consolidated = consolidated;
	}

	protected void setConfidence(double confidence) {
		this.confidence = confidence;
	}

	protected void setAttempts(long attempts) {
		this.attempts = attempts;
	}

	protected void setDeadCounter(int counter) {
		this.deadCounter = counter;
	}

	protected void setLabelsSize(int size) {
		// Do nothing
	}

	protected void setCenter(Point center) {
		this.center = center;
	}

}