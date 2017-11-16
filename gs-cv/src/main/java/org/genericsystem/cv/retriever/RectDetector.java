package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class RectDetector {
	private Img img;

	public RectDetector(Img img) {
		this.img = img;
	}

	public List<Rect> getRects(int minArea, int blockSize, double c, Size close) {
		return applyNoOverlapsConstraint(getRects(getClosed(blockSize, c, close), minArea));
	}

	private Img getClosed(int blockSize, double c, Size close) {
		return img.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(blockSize, c).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, close);
	}

	private List<Rect> getRects(Img closed, int minArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		return contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList());
	}

	private List<Rect> applyNoOverlapsConstraint(List<Rect> rects) {
		List<Rect> result = new ArrayList<>();
		for (int i = rects.size() - 1; i > 0; --i) {
			Rect rect = rects.get(i);
			if (!hasOverlapsAfterIdx(rects, i))
				result.add(rect);
		}
		return result;
	}

	private boolean hasOverlapsAfterIdx(List<Rect> rects, int idx) {
		Rect rect = rects.get(idx);
		for (int j = idx - 1; j > 0; --j) {
			Rect r = rects.get(j);
			if (isOverlapping(r, rect)) {
				return true;
			}
		}
		return false;
	}

	private boolean isOverlapping(Rect rect1, Rect rect2) {
		GSRect gr1 = new GSRect(rect1.x, rect1.y, rect1.width, rect1.height);
		GSRect gr2 = new GSRect(rect2.x, rect2.y, rect2.width, rect2.height);
		return gr1.isOverlapping(gr2);
	}
}