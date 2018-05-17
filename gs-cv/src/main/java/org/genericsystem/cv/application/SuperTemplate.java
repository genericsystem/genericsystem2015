package org.genericsystem.cv.application;

import java.util.List;

import org.genericsystem.cv.Img;
import org.genericsystem.layout.Layout;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import com.google.common.base.Function;

public class SuperTemplate extends SuperFrameImg {
	private final int type;

	public SuperTemplate(SuperFrameImg superFrame, int displayType, Function<SuperFrameImg, Img> retriever) {
		super(retriever.apply(superFrame).getSrc(), superFrame.getPp(), superFrame.getF());
		this.type = displayType;
	}

	@Override
	protected Img buildDisplay() {
		return new Img(new Mat(size(), type, Scalar.all(0)), false);
	}

	public Layout layout() {
		return getFrame().buildLayout(new Size(2, 0.4), new Size(0.04, 0.008), 8);
	}

	public void drawLayout(Layout layout) {
		layout.draw(getDisplay(), new Scalar(255, 0, 0), new Scalar(0, 0, 255), 1, -1);
	}

	public void drawCenterPoints(List<Rect> referenceRects, Scalar color, int thickNess, int radius) {
		referenceRects.forEach(rect -> Imgproc.circle(getDisplay().getSrc(), new Point((rect.tl().x + rect.br().x) / 2, (rect.tl().y + rect.br().y) / 2), radius, color, thickNess));
	}

	public void drawCentroids(List<Point> referenceRects, Scalar color, int thickNess, int radius) {
		referenceRects.forEach(pt -> Imgproc.circle(getDisplay().getSrc(), pt, radius, color, thickNess));
	}

}
