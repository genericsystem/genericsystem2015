package org.genericsystem.cv.application;

import org.genericsystem.cv.Img;
import org.genericsystem.layout.Layout;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

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
		return getFrame().buildLayout(new Size(0.01, 0.01), 8);
	}

	public void drawLayout(Layout layout) {
		layout.draw(getDisplay(), new Scalar(255, 0, 0), new Scalar(0, 0, 255), 3, -1);
	}

}
