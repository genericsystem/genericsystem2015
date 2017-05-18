package org.genericsystem.cv;

import java.util.Arrays;
import java.util.stream.Stream;

import org.opencv.core.Core;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class RotatedZone {

	private final RotatedRect rect;

	public RotatedZone(RotatedRect rect) {
		this.rect = rect;
	}

	public RotatedRect getRect() {
		return rect;
	}

	public RotatedZoneScorer newUnsupervisedScorer(Stream<Img> imgs) {
		return new RotatedZoneScorer(this, imgs);
	}

	public RotatedZone adjustRect(double dx, double dy, int maxWidht, int maxHeight) {
		return new RotatedZone(new RotatedRect(rect.center, new Size(rect.size.width + 2 * dx, rect.size.height + 2 * dy), rect.angle));
	}

	public void draw(Img img, Scalar color, int thickness) {
		Point[] result = new Point[4];
		rect.points(result);
		Imgproc.drawContours(img.getSrc(), Arrays.asList(new MatOfPoint(result)), 0, color, thickness);
	}

	public void write(Img img, String text, double fontScale, Scalar color, int thickness) {
		Imgproc.putText(img.getSrc(), text, new Point(rect.center.x - rect.size.width / 2, rect.center.y - rect.size.height / 2), Core.FONT_HERSHEY_PLAIN, fontScale, color, thickness);
	}

	public String ocr(Img img) {
		return Ocr.doWork(img.getSrc(), getRect());
	}

}
