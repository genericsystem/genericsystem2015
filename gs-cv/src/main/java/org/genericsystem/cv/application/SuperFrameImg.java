package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Calibrated;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Lines;
import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Lines.Line;
import org.genericsystem.cv.lm.LevenbergImpl;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class SuperFrameImg {

	private final Img frame;
	private final double[] pp;
	private final double f;

	private Img display;

	private Img bilateralFilter;
	private Img binarized;
	private Img gradient;
	private Img diffFrame;
	private Img binaryClosed10;
	private Img binaryClosed20;
	private Img binaryClosed30;
	private Img binaryClosed40;

	public static SuperFrameImg create(VideoCapture capture, double f) {
		Mat frameMat = new Mat();
		capture.read(frameMat);
		return new SuperFrameImg(frameMat, new double[] { frameMat.width() / 2, frameMat.height() / 2 }, f);
	}

	public SuperFrameImg(Mat frameMat, double[] pp, double f) {
		frame = new Img(frameMat, true);
		this.pp = pp;
		this.f = f;
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
		return getBilateralFilter().adaptativeGaussianInvThreshold(11, 3);
	}

	private Img buildGradient() {
		return getFrame().bgr2Gray().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(2, 2)).thresHold(15, 255, Imgproc.THRESH_BINARY);
	}

	protected Img buildDisplay() {
		return new Img(getFrame().getSrc(), true);
	}

	private Img buildBinaryClosed10() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(10, 10));
	}

	private Img buildBinaryClosed20() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(20, 20)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(20, 20));
	}

	private Img buildBinaryClosed30() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(30, 30));
	}

	private Img buildBinaryClosed40() {
		return getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(40, 40)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(40, 40));
	}

	public Lines detectLines() {
		// Img grad = getDiffFrame().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		// Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 10, 3));
		// Img grad2 = getDiffFrame().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		// lines.lines.addAll(new Lines(grad2.houghLinesP(1, Math.PI / 180, 10, 30, 9)).lines);
		// Img grad = getBinaryClosed10().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		// Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 10, 3));
		// Img grad2 = getBinaryClosed30().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		// lines.lines.addAll(new Lines(grad2.houghLinesP(1, Math.PI / 180, 10, 30, 8)).lines);
		Img grad = getGradient().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(8, 8)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(8, 8)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 10, 3));
		Img grad2 = getGradient().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(30, 30)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE,
				new Size(3, 3));
		lines.getLines().addAll(new Lines(grad2.houghLinesP(1, Math.PI / 180, 10, 30, 10)).getLines());
		return lines;
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

	public Mat findHomography(AngleCalibrated[] calibrateds) {

		double[][] vps = new double[][] { calibrateds[0].getCalibratexyz(), calibrateds[1].getCalibratexyz(), calibrateds[2].getCalibratexyz() };
		// System.out.println("vps : " + Arrays.deepToString(vps));

		double[][] vps2D = getVp2DFromVps(vps);
		// System.out.println("vps2D : " + Arrays.deepToString(vps2D));

		// System.out.println("vp1 " + calibrateds[0]);
		// System.out.println("vp2 " + calibrateds[1]);
		// System.out.println("vp3 " + calibrateds[2]);

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

	public double[][] getVp2DFromVps(double vps[][]) {
		double[][] result = new double[3][3];
		for (int i = 0; i < 3; i++) {
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

	public void drawVanishingPointLines(Lines lines, AngleCalibrated calibratedVp, Scalar color, int thickness) {
		double[] uncalibrate0 = calibratedVp.uncalibrate(pp, f);
		Lines horizontals = lines.filter(line -> AngleCalibrated.distance(uncalibrate0, line) < 0.4);
		draw(horizontals, color, thickness);
	}

	public SuperTemplate deperspective(Mat homography) {
		return new SuperTemplate(warpPerspective(homography).getSrc(), pp, f);
	}

	public AngleCalibrated findVanishingPoint(Lines lines, AngleCalibrated old) {
		double[] thetaPhi = new LevenbergImpl<>((line, params) -> new AngleCalibrated(params).distance(line, pp, f), lines.getLines(), old.getThetaPhi()).getParams();
		return new AngleCalibrated(thetaPhi);
	}

	public List<Line> findTextOrientationLines() {
		return TextOrientationLinesDetector.getTextOrientationLines(this);
	}

	public void drawVpsArrows(Calibrated[] calibratedVps, double[] shift, Scalar color, int thickness) {
		drawArrow(new Point(shift[0], shift[1]), new Point(shift[0] + calibratedVps[0].getX() * 20, shift[1] + calibratedVps[0].getY() * 20), new Scalar(0, 255, 0), 2);
		drawArrow(new Point(shift[0], shift[1]), new Point(shift[0] - calibratedVps[1].getX() * 20, shift[1] - calibratedVps[1].getY() * 20), new Scalar(255, 0, 0), 2);
		drawArrow(new Point(shift[0], shift[1]), new Point(shift[0] - calibratedVps[2].getX() * 20, shift[1] - calibratedVps[2].getY() * 20), new Scalar(0, 0, 255), 2);
	}

	public void drawArrow(Point pt1, Point pt2, Scalar color, int thickness) {
		Imgproc.arrowedLine(getDisplay().getSrc(), pt1, pt2, color, thickness, 8, 0, 0.5);
	}

	public void drawDetectedRects() {
		drawRects(detectRects(), new Scalar(255), -1);
	}

	public void drawRects(List<Rect> gsRects, Scalar color, int thickness) {
		Mat display = getDisplay().getSrc();
		gsRects.forEach(rect -> Imgproc.rectangle(display, rect.tl(), rect.br(), color, thickness));
	}

	public Img getGrayFrame() {
		return CvType.CV_8U != getFrame().type() ? getFrame().bgr2Gray() : getFrame();
	}

	private Img buildDiffFrame() {
		Mat diffFrame = getGrayFrame().gaussianBlur(new Size(5, 5)).getSrc();
		Core.absdiff(diffFrame, new Scalar(90), diffFrame);
		Imgproc.adaptiveThreshold(diffFrame, diffFrame, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);
		return new Img(diffFrame, false).cleanTablesInv(0.05);
	}

	public List<Rect> detectRects() {
		return detectRects(getDiffFrame(), 5, 10000);
		// return (detectRects(getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(7,3)), 5,10000));

		// List<GSRect> rects = getRects(200, 11, 3, new Size(11, 3));
		// List<GSRect> children = getRects(40, 17, 3, new Size(7, 3));
		// return cleanList(rects, children, 0.70);
	}

	List<Rect> detectRects(Img binarized, int minArea, int maxArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(binarized.getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		Size size = binarized.size();
		return contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea && Imgproc.contourArea(contour) < maxArea).map(c -> Imgproc.boundingRect(c))
				.filter(rect -> rect.tl().x != 0 && rect.tl().y != 0 && rect.br().x != (size.width) && rect.br().y != (size.height)).collect(Collectors.toList());
	}

	public List<GSRect> cleanList(List<GSRect> bigRects, List<GSRect> smallRects, double overlapThreshold) {
		smallRects.removeIf(smallRect -> bigRects.stream().anyMatch(bigRect -> smallRect.inclusiveArea(bigRect) > overlapThreshold));
		return Stream.concat(smallRects.stream().filter(smallRect -> bigRects.stream().filter(rect -> rect.isOverlapping(smallRect)).noneMatch(rect -> rect.getInsider(smallRect) == null)), bigRects.stream()).collect(Collectors.toList());
	}

	private List<Rect> applyNoOverlapsConstraint(List<Rect> rects) {
		Collections.reverse(rects);
		List<Rect> result = new ArrayList<>();
		for (ListIterator<Rect> it = rects.listIterator(); it.hasNext();) {
			int i = it.nextIndex();
			Rect rect = it.next();
			if (rects.subList(i, rects.size() - 1).stream().filter(r -> r != rect).noneMatch(r -> isOverlapping(r, rect)))
				result.add(rect);
		}
		return result;
	}

	public boolean isOverlapping(Rect rect, Rect other) {
		return rect.x <= other.br().x && other.tl().x <= rect.br().x && rect.y <= other.br().y && other.tl().y <= rect.br().y;
	}

	public AngleCalibrated[] findOtherVps(AngleCalibrated calibrated0, Lines lines) {
		// System.out.println(Arrays.toString(calibrated0.getCalibratexyz()));
		AngleCalibrated[] result = new AngleCalibrated[] { null, null, null };
		double bestError = Double.MAX_VALUE;
		// double bestAngle = 0;
		for (double angle = 0; angle < 360 / 180 * Math.PI; angle += 1 * Math.PI / 180) {
			AngleCalibrated calibratexy = calibrated0.getOrthoFromAngle(angle);
			AngleCalibrated calibratez = calibrated0.getOrthoFromVps(calibratexy);
			if (calibratexy.getPhi() < calibratez.getPhi()) {
				AngleCalibrated tmp = calibratexy;
				calibratexy = calibratez;
				calibratez = tmp;
			}
			double error = calibratexy.distance(lines.getLines(), pp, f);
			if (error < bestError) {
				bestError = error;
				result[0] = calibrated0;
				result[1] = calibratexy;
				result[2] = calibratez;
				// bestAngle = angle;
			}
		}
		double theta0 = Math.abs(result[0].getTheta()) % Math.PI;
		theta0 = Math.min(Math.PI - theta0, theta0);
		double theta1 = Math.abs(result[1].getTheta()) % Math.PI;
		theta1 = Math.min(Math.PI - theta1, theta1);
		if (theta0 > theta1) {
			AngleCalibrated tmp = result[0];
			result[0] = result[1];
			result[1] = tmp;
		}
		return result;
	}

	public double[] getPp() {
		return pp;
	}

	public double getF() {
		return f;
	}

}
