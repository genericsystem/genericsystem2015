package org.genericsystem.cv;

import java.io.File;
import java.util.stream.Stream;

import org.genericsystem.cv.comparator.ZoneScorer2;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Zone {
	private Rect rect;
	private int num;

	public Zone() {
	}

	public Zone(int num, Rect rect) {
		this.num = num;
		this.rect = rect;
	}

	public Rect getRect() {
		return rect;
	}

	public ZoneScorer newUnsupervisedScorer(Stream<Img> imgs) {
		return new ZoneScorer(this, imgs);
	}
	
	public ZoneScorer2 newUnsupervisedScorer2(File file, Stream<Img> imgs) {
		return new ZoneScorer2(this, imgs, file);
	}
	
	public ZoneScorer2 createUnsupervisedScorer(Stream<File> files) {
		return new ZoneScorer2(this, files);
	}

	public Zone adjustRect(double dx, double dy, int maxWidht, int maxHeight) {
		Point tl = new Point(rect.tl().x > dx ? rect.tl().x - dx : 0d, rect.tl().y > dy ? rect.tl().y - dy : 0d);
		Point br = new Point((rect.br().x + dx > maxWidht) ? maxWidht : rect.br().x + dx, (rect.br().y + dy > maxHeight) ? maxHeight : rect.br().y + dy);
		return new Zone(num, new Rect(tl, br));
	}

	public void draw(Img img, Scalar color, int thickness) {
		Imgproc.rectangle(img.getSrc(), rect.tl(), rect.br(), color, thickness);
	}

	public void write(Img img, String text, double fontScale, Scalar color, int thickness) {
		Imgproc.putText(img.getSrc(), text, new Point(rect.tl().x, rect.br().y), Core.FONT_HERSHEY_PLAIN, fontScale, color, thickness);
	}

	public String ocr(Img img) {
		return Ocr.doWork(new Mat(img.getSrc(), getRect()));
	}

	public int getNum() {
		return num;
	}

	public Img getRoi(Img img) {
		return new Img(img, this);
	}

	public Img getImg(Img img) {
		return new Img(new Mat(img.getSrc(), getRect()));
	}

	public void draw(Mat roi, Scalar color, int thickness) {
		Imgproc.rectangle(roi, rect.tl(), rect.br(), color, thickness);
	}

}
