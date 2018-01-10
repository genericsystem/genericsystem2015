package org.genericsystem.cv;

import java.util.Arrays;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Deperspectiver.Lines;
import org.genericsystem.cv.lm.LevenbergImpl;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
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

	public Img warpPerspective(Mat homography) {
		Mat deperspectived = new Mat();
		Imgproc.warpPerspective(getFrame().getSrc(), deperspectived, homography, size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
		return new Img(deperspectived, false);
	}

	public Mat findHomography(AngleCalibrated[] calibrateds, double[] pp, double f) {

		double[][] vps = new double[][] { calibrateds[0].getCalibratexyz(), calibrateds[1].getCalibratexyz(), calibrateds[2].getCalibratexyz() };
		// System.out.println("vps : " + Arrays.deepToString(vps));

		double[][] vps2D = getVp2DFromVps(vps, pp, f);
		System.out.println("vps2D : " + Arrays.deepToString(vps2D));

		System.out.println("vp1 " + calibrateds[0]);
		System.out.println("vp2 " + calibrateds[1]);
		System.out.println("vp3 " + calibrateds[2]);

		double theta = calibrateds[0].getTheta();
		double theta2 = calibrateds[1].getTheta();
		Size size = size();
		double x = size().width / 6;

		double[] A = new double[] { size.width / 2, size.height / 2, 1 };
		double[] B = new double[] { size.width / 2 + (Math.cos(theta) < 0 ? -x : x), size.height / 2 };
		double[] D = new double[] { size.width / 2, size.height / 2 + (Math.sin(theta2) < 0 ? -x : +x), 1 };
		double[] C = new double[] { size.width / 2 + (Math.cos(theta) < 0 ? -x : +x), size.height / 2 + (Math.sin(theta2) < 0 ? -x : +x) };

		double[] A_ = A;
		double[] B_ = new double[] { size.width / 2 + x * vps[0][0], size.height / 2 + x * vps[0][1], 1 };
		double[] D_ = new double[] { size.width / 2 + x * vps[1][0], size.height / 2 + x * vps[1][1], 1 };
		double[] C_ = cross2D(cross(B_, vps2D[1]), cross(D_, vps2D[0]));

		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(new Point(A_), new Point(B_), new Point(C_), new Point(D_)), new MatOfPoint2f(new Point(A), new Point(B), new Point(C), new Point(D)));
	}

	public static double[][] getVp2DFromVps(double vps[][], double[] pp, double f) {
		double[][] result = new double[2][3];
		for (int i = 0; i < 2; i++) {
			result[i][0] = vps[i][0] * f / vps[i][2] + pp[0];
			result[i][1] = vps[i][1] * f / vps[i][2] + pp[1];
			result[i][2] = 1.0;
		}
		return result;
	}

	static double[] cross2D(double[] a, double b[]) {
		return on2D(cross(a, b));
	}

	static double[] on2D(double[] a) {
		return new double[] { a[0] / a[2], a[1] / a[2], 1 };
	}

	static double[] cross(double[] a, double b[]) {
		return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
	}

	public void draw(Lines lines, Scalar color, int thickness) {
		lines.draw(getDisplay().getSrc(), color, thickness);
	}

	public void draw(AngleCalibrated calibratedVps, Lines lines, double[] pp, double f, Scalar color, int thickness) {
		double[] uncalibrate0 = calibratedVps.uncalibrate(pp, f);
		Lines horizontals = lines.filter(line -> AngleCalibrated.distance(uncalibrate0, line) < 0.3);
		draw(horizontals, color, thickness);
	}

	public Lines detectLines() {
		Img grad = getBinaryClosed10().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 10, 3));
		Img grad2 = getBinaryClosed30().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		lines.lines.addAll(new Lines(grad2.houghLinesP(1, Math.PI / 180, 10, 30, 10)).lines);
		return lines;
	}

	public Img dePerspective(AngleCalibrated[] calibratedVps, double[] pp, double f) {
		return warpPerspective(findHomography(calibratedVps, pp, f));
	}

	public AngleCalibrated findVanishingPoint(Lines lines, AngleCalibrated old, double[] pp, double f) {
		double[] thetaPhi = new LevenbergImpl<>((line, params) -> new AngleCalibrated(params).distance(line, pp, f), lines.lines, old.getThetaPhi()).getParams();
		return new AngleCalibrated(thetaPhi);
	}

}
