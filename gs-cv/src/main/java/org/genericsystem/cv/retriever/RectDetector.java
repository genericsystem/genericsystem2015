package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class RectDetector {
	private Img img;

	public RectDetector(Img img) {
		this.img = img;
	}

	public List<GSRect> getRects(int minArea, int blockSize, double c, Size close) {
		return applyNoOverlapsConstraint(getRects(getClosed(blockSize, c, close), minArea));
	}

	private Img getClosed(int blockSize, double c, Size close) {
		return img.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(blockSize, c).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, close);
	}

	private List<GSRect> getRects(Img closed, int minArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		return RectToolsMapper.rectToGSRect(contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList()));
	}

	private List<GSRect> applyNoOverlapsConstraint(List<GSRect> rects) {
		Collections.reverse(rects);
		List<GSRect> result = new ArrayList<>();
		for (ListIterator<GSRect> it = rects.listIterator(); it.hasNext();) {
			int i = it.nextIndex();
			GSRect rect = it.next();
			if (rects.subList(i, rects.size() - 1).stream().filter(r -> r != rect).noneMatch(r -> r.isOverlapping(rect)))
				result.add(rect);
		}
		return result;
	}

}