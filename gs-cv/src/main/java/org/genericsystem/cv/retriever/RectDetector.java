package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;
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
	private static final int MIN_AREA = 200;
	private static final int MIN_AREA_SMALL = 40;

	public RectDetector(Img img) {
		this.img = img;
	}

	public List<Rect> getFilteredRects(double thresholdFactor) {
		// return filterRects(getRects(), thresholdFactor);
		return applyConstraint(getRects());
	}

	public List<Rect> getFilteredRects2(double thresholdFactor) {
		// return filterRects(getRects2(), thresholdFactor);
		return applyConstraint(getRects2());
	}

	public List<Rect> getRects() {
		return getRects(getClosed(), MIN_AREA);
	}

	public List<Rect> getRects2() {
		return getRects(getClosed2(), MIN_AREA_SMALL);
	}

	private List<Rect> getRects(Img closed, int minArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		return contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList());
	}

	private Img getClosed() {
		return img.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(11, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11, 3));
	}

	private Img getClosed2() {
		return img.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(7, 3));
	}

	private List<Rect> filterRects(List<Rect> rects, double thresholdFactor) {
		// TODO Delete overlapping rects (bottom up)
		double meanArea = rects.stream().mapToDouble(r -> r.area()).average().getAsDouble();
		double sem = Math.sqrt(rects.stream().mapToDouble(r -> Math.pow(r.area() - meanArea, 2)).sum() / (rects.size() - 1));
		return rects.stream().filter(r -> (r.area() - meanArea) <= (sem * thresholdFactor)).collect(Collectors.toList());
	}

	private List<Rect> filter(List<Rect> rects) {
		List<Rect> newRects = new ArrayList<>(rects);
		StringBuffer sb = new StringBuffer();
		sb.append("size before: ").append(rects.size());

		Iterator<Rect> it = newRects.iterator();
		while (it.hasNext()) {
			Rect rect = it.next();
			Predicate<Rect> predicate = other -> rect.tl().x < other.br().x && other.tl().x < rect.br().x && rect.tl().y < other.br().y && other.tl().y < rect.br().y;
			boolean ok = rects.stream().filter(r -> !r.equals(rect)).anyMatch(predicate);
			if (ok)
				it.remove();
		}
		sb.append(" | size after: ").append(newRects.size());
		System.err.println(sb.toString());
		return newRects;
	}

	private List<Rect> applyConstraint(List<Rect> rects) {
		System.err.println("initial size: " + rects.size());
		List<Rect> result = new ArrayList<>();
		for (int i = rects.size() - 1; i > 0; --i) {
			Rect rect = rects.get(i);
			boolean brokenConstraint = false;
			for (int j = i - 1; j > 0; --j) {
				Rect r = rects.get(j);
				if (isOverlapping(r, rect)) {
					brokenConstraint = true;
					break;
				}
			}
			if (!brokenConstraint)
				result.add(rect);
		}
		System.err.println("after applyConstraint: " + result.size());
		return result;
	}

	private boolean isOverlapping(Rect rect1, Rect rect2) {
		GSRect gr1 = new GSRect(rect1.x, rect1.y, rect1.width, rect1.height);
		GSRect gr2 = new GSRect(rect2.x, rect2.y, rect2.width, rect2.height);
		return gr1.isOverlapping(gr2);
	}
}