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
	private static final int MIN_AREA = 200;
	private static final int MIN_AREA_SMALL = 40;

	public RectDetector(Img img) {
		this.img = img;
	}

	public List<Rect> getRects() {
		return applyConstraint(getRects(getClosed(), MIN_AREA));
	}

	public List<Rect> getRects2() {
		return applyConstraint(getRects(getClosed2(), MIN_AREA_SMALL));
	}

	private Img getClosed() {
		return img.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(11, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11, 3));
	}

	private Img getClosed2() {
		return img.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(7, 3));
	}

	private List<Rect> getRects(Img closed, int minArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		return contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList());
	}

	private List<Rect> applyConstraint(List<Rect> rects) {
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
		return result;
	}

	private boolean isOverlapping(Rect rect1, Rect rect2) {
		GSRect gr1 = new GSRect(rect1.x, rect1.y, rect1.width, rect1.height);
		GSRect gr2 = new GSRect(rect2.x, rect2.y, rect2.width, rect2.height);
		return gr1.isOverlapping(gr2);
	}
}