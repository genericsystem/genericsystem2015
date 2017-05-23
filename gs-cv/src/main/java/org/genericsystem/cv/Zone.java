package org.genericsystem.cv;

import java.util.stream.Stream;

import org.opencv.core.Core;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Zone {

	private final Rect rect;

	public Zone(Rect rect) {
		this.rect = rect;
	}

	public Rect getRect() {
		return rect;
	}

	public ZoneScorer newUnsupervisedScorer(Stream<Img> imgs) {
		return new ZoneScorer(this, imgs);
	}

	public Zone adjustRect(double dx, double dy, int maxWidht, int maxHeight) {
		Point tl = new Point(rect.tl().x > dx ? rect.tl().x - dx : 0d, rect.tl().y > dy ? rect.tl().y - dy : 0d);
		Point br = new Point((rect.br().x + dx > maxWidht) ? maxWidht : rect.br().x + dx,
				(rect.br().y + dy > maxHeight) ? maxHeight : rect.br().y + dy);
		return new Zone(new Rect(tl, br));
	}

	public void draw(Img img, Scalar color, int thickness) {
		Imgproc.rectangle(img.getSrc(), rect.tl(), rect.br(), color, thickness);
	}

	public void write(Img img, String text, double fontScale, Scalar color, int thickness) {
		Imgproc.putText(img.getSrc(), text, new Point(rect.tl().x, rect.br().y), Core.FONT_HERSHEY_PLAIN, fontScale, color, thickness);
	}

	public String ocr(Img img) {
		return Ocr.doWork(img.getSrc(), getRect());
	}

}
