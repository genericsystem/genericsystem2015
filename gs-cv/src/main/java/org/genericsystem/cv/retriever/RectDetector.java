package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
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
		return filterRects(getRects(), thresholdFactor);
	}

	public List<Rect> getFilteredRects2(double thresholdFactor) {
		return filterRects(getRects2(), thresholdFactor);
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
		double meanArea = rects.stream().mapToDouble(r -> r.area()).average().getAsDouble();
		double sem = Math.sqrt(rects.stream().mapToDouble(r -> Math.pow(r.area() - meanArea, 2)).sum() / (rects.size() - 1));
		return rects.stream().filter(r -> (r.area() - meanArea) <= (sem * thresholdFactor)).collect(Collectors.toList());
	}
}