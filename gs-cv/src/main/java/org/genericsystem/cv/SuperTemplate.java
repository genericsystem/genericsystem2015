package org.genericsystem.cv;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.SuperFrameImg;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;

public class SuperTemplate extends SuperFrameImg {
	public SuperTemplate(SuperFrameImg superFrame) {
		this(superFrame.getDisplay().getSrc(), superFrame.getPp(), superFrame.getF());
	}

	public SuperTemplate(Mat frameMat, double[] pp, double f) {
		super(frameMat, pp, f);
	}

	@Override
	protected Img buildDisplay() {
		return new Img(new Mat(size(), CvType.CV_8UC1, new Scalar(0)), false);
	}

}
