package org.genericsystem.cv.classifier;

import java.text.Normalizer;
import java.util.Map;
import java.util.Optional;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ModelTools;
import org.opencv.core.Core;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class DocField extends AbstractField {
	private int num;
	private String uid;

	public DocField(int num, Rect rect) {
		super(rect);
		this.num = num;
		this.uid = ModelTools.generateZoneUID(rect);
	}

	// Draw without homography
	public void drawOcrPerspectiveInverse(Img display, Scalar color, int thickness) {
		MatOfPoint2f results = new MatOfPoint2f(center, new Point(rect.x, rect.y), new Point(rect.x + rect.width - 1, rect.y), new Point(rect.x + rect.width - 1, rect.y + rect.height - 1), new Point(rect.x, rect.y + rect.height - 1));
		Point[] targets = results.toArray();
		Imgproc.line(display.getSrc(), targets[1], targets[2], color, thickness);
		Imgproc.line(display.getSrc(), targets[2], targets[3], color, thickness);
		Imgproc.line(display.getSrc(), targets[3], targets[4], color, thickness);
		Imgproc.line(display.getSrc(), targets[4], targets[1], color, thickness);
		Point topCenter = new Point((targets[1].x + targets[2].x) / 2, (targets[1].y + targets[2].y) / 2);
		double l = Math.sqrt(Math.pow(targets[1].x - topCenter.x, 2) + Math.pow(targets[1].y - topCenter.y, 2));
		Imgproc.line(display.getSrc(), new Point(topCenter.x, topCenter.y - 2), new Point(topCenter.x, topCenter.y - 20), new Scalar(0, 255, 0), 1);
		Imgproc.putText(display.getSrc(), Normalizer.normalize(consolidated.orElse(""), Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", ""), new Point(topCenter.x - l, topCenter.y - 22), Core.FONT_HERSHEY_TRIPLEX, 1.0, color, thickness);
		results.release();
	}

	public void drawRect(Img img, Scalar color, int thickness) {
		Imgproc.rectangle(img.getSrc(), rect.tl(), rect.br(), color, thickness);
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
		this.rect = rect;
		this.center = new Point(rect.x + rect.width / 2, rect.y + rect.height / 2);
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

}