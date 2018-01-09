package org.genericsystem.cv;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class SuperFrameImg {

	private Img frame;
	private Img display;

	private Img bilateralFilter;
	private Img binarized;
	private Img gradient;
	private Img diffFrame;
	private Img binaryClosed10;
	private Img binaryClosed20;
	private Img binaryClosed30;
	private Img binaryClosed40;

	public static SuperFrameImg create(VideoCapture capture) {
		Mat frameMat = new Mat();
		capture.read(frameMat);
		return new SuperFrameImg(frameMat);
	}

	public SuperFrameImg(Mat frameMat) {
		frame = new Img(frameMat, true);
	}

	public Img getFrame() {
		return frame;
	}

	public Img getBilateralFilter() {
		return bilateralFilter != null ? bilateralFilter : (bilateralFilter = buildBilateralFilter());
	}

	public Img getBinarized() {
		return binarized != null ? binarized : (binarized = buildBinarized());
	}

	public Img getGradient() {
		return gradient != null ? gradient : (gradient = buildGradient());
	}

	public Img getDisplay() {
		return display != null ? display : (display = buildDisplay());
	}

	public Img getDiffFrame() {
		return diffFrame != null ? diffFrame : (diffFrame = buildDiffFrame());
	}

	public Img getBinaryClosed10() {
		return binaryClosed10 != null ? binaryClosed10 : (binaryClosed10 = buildBinaryClosed10());
	}

	public Img getBinaryClosed20() {
		return binaryClosed20 != null ? binaryClosed20 : (binaryClosed20 = buildBinaryClosed20());
	}

	public Img getBinaryClosed30() {
		return binaryClosed30 != null ? binaryClosed30 : (binaryClosed30 = buildBinaryClosed30());
	}

	public Img getBinaryClosed40() {
		return binaryClosed40 != null ? binaryClosed40 : (binaryClosed40 = buildBinaryClosed40());
	}

	private Img buildBilateralFilter() {
		return getFrame().bilateralFilter();
	}

	private Img buildBinarized() {
		return getBilateralFilter().adaptativeGaussianInvThreshold(3, 3);
	}

	private Img buildGradient() {
		return getFrame().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(2, 2)).otsu();
	}

	private Img buildDisplay() {
		return new Img(getFrame().getSrc(), true);
	}

	private Img buildDiffFrame() {
		Mat diffFrame = new Mat();
		Imgproc.cvtColor(frame.getSrc(), diffFrame, Imgproc.COLOR_BGR2GRAY);
		Imgproc.GaussianBlur(diffFrame, diffFrame, new Size(5, 5), 0);
		Core.absdiff(diffFrame, new Scalar(255), diffFrame);
		Imgproc.adaptiveThreshold(diffFrame, diffFrame, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 5, 3);
		return new Img(diffFrame, false);
	}

	private Img buildBinaryClosed10() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10));
	}

	private Img buildBinaryClosed20() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(20, 20));
	}

	private Img buildBinaryClosed30() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30));
	}

	private Img buildBinaryClosed40() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(40, 40));
	}

	public double[] getPrincipalPoint() {
		return new double[] { frame.width() / 2, frame.height() / 2 };
	}

	public Size size() {
		return getFrame().size();
	}

	public double width() {
		return getFrame().width();
	}

	public double height() {
		return getFrame().height();
	}

}
