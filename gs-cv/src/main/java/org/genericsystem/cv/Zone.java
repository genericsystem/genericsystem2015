package org.genericsystem.cv;

import java.io.Serializable;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Zone implements Serializable {
	private Rect rect;
	private int num;

	private String label;

	public Zone() {
	}

	public Zone(int num, Rect rect) {
		this.num = num;
		this.rect = rect;
	}

	public Rect getRect() {
		return rect;
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
		return new Img(new Mat(img.getSrc(), getRect()), false);
	}

	public void draw(Mat roi, Scalar color, int thickness) {
		Imgproc.rectangle(roi, rect.tl(), rect.br(), color, thickness);
	}

	@Override
	public String toString() {
		return "Zone [rect=" + rect + ", num=" + num + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + num;
		result = prime * result + ((rect == null) ? 0 : rect.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Zone other = (Zone) obj;
		if (num != other.num)
			return false;
		if (rect == null) {
			if (other.rect != null)
				return false;
		} else if (!rect.equals(other.rect))
			return false;
		return true;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

}
