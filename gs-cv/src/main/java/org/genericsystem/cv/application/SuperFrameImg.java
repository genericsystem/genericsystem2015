package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Calibrated;
import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Lines;
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
import org.opencv.imgproc.Moments;

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

	public SuperFrameImg(Mat frameMat, double[] pp, double f) {
		frame = new Img(frameMat, false);
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
		return new SuperTemplate(this, CvType.CV_8UC3, st -> st.warpPerspective(homography));
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

	@SuppressWarnings("resource")
	private Img buildDiffFrame() {
		Mat diffFrame = getGrayFrame().gaussianBlur(new Size(5, 5)).getSrc();
		Core.absdiff(diffFrame, new Scalar(0), diffFrame);
		Imgproc.adaptiveThreshold(diffFrame, diffFrame, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);
		return new Img(diffFrame, false);// .cleanTablesInv(0.05);
	}

	public List<Rect> detectRects() {
		return detectRects(getDiffFrame(), 10, 10000, 0.3);
	}

	public List<Point> detectCentroids() {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(getDiffFrame().getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		List<Point> result = new ArrayList<>();
		for (MatOfPoint contour : contours) {
			Moments moments = Imgproc.moments(contour);
			result.add(new Point(moments.m10 / moments.m00, moments.m01 / moments.m00));
		}
		Collections.reverse(result);
		return result;
	}

	public int countWhitePixels(MatOfPoint contour, Rect rect, Img img) {
		Mat mask = Mat.zeros(rect.size(), CvType.CV_8UC1);
		Imgproc.drawContours(mask, Arrays.asList(contour), 0, new Scalar(255), -1, Imgproc.LINE_8, new Mat(), Integer.MAX_VALUE, new Point(-rect.tl().x, -rect.tl().y));
		Imgproc.drawContours(mask, Arrays.asList(contour), 0, new Scalar(0), 1, Imgproc.LINE_8, new Mat(), Integer.MAX_VALUE, new Point(-rect.tl().x, -rect.tl().y));
		int white = 0;
		for (int row = 0; row < mask.rows(); row++)
			for (int col = 0; col < mask.cols(); col++)
				if (mask.get(row, col)[0] != 0 && img.get(row + (int) rect.tl().y, col + (int) rect.tl().x)[0] != 0)
					white++;
		return white;
	}

	public List<SuperContour> detectSuperContours(double minArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Mat hierarchy = new Mat();
		Img img = getDiffFrame().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(1, 1));
		Imgproc.findContours(img.getSrc(), contours, hierarchy, Imgproc.RETR_TREE, Imgproc.CHAIN_APPROX_NONE);
		List<SuperContour> result = new ArrayList<>();
		int row = 0;
		for (MatOfPoint contour : contours) {
			double[] indexes = hierarchy.get(0, row);
			double fatherIndex = indexes[3];
			// double prevBrotherIndex = indexes[1];
			// double nextBrotherIndex = indexes[0];
			// double childIndex = indexes[2];
			MatOfPoint fatherWrapper = fatherIndex != -1 ? contours.get((int) fatherIndex) : null;
			if (fatherWrapper == null || Imgproc.contourArea(contour) > minArea) {
				if (countWhitePixels(contour, Imgproc.boundingRect(contour), img) != 0)
					result.add(new SuperContour(contour, hierarchy.get(0, row)[3] == -1));
			}
			row++;
		}
		Collections.sort(result);

		return result;
	}

	public static class SuperContour implements Comparable<SuperContour> {

		public final MatOfPoint contour;
		public SuperContour succ;
		public SuperContour pred;
		public Point center;
		public Point tangent;
		public Point antiTangent;
		public double angle;
		public Point left;
		public Point right;
		public Rect rect;
		public double lxmin, lxmax;
		public double lymin, lymax;
		public final boolean isLeaf;

		SuperContour(MatOfPoint contour, boolean isLeaf) {

			this.contour = contour;
			this.rect = Imgproc.boundingRect(contour);
			double tx;
			double ty;
			Moments moments = Imgproc.moments(contour);
			this.center = new Point(moments.m10 / moments.m00, moments.m01 / moments.m00);
			if (rect.width < (2 * rect.height) || Imgproc.contourArea(contour) < 100) {
				tx = 1;
				ty = 0;
			} else {
				Mat momentsMatrix = new Mat(new Size(2, 2), CvType.CV_64FC1);
				momentsMatrix.put(0, 0, moments.mu20 / moments.m00);
				momentsMatrix.put(0, 1, moments.mu11 / moments.m00);
				momentsMatrix.put(1, 0, moments.mu11 / moments.m00);
				momentsMatrix.put(1, 1, moments.mu02 / moments.m00);
				Mat svdU = new Mat();
				Core.SVDecomp(momentsMatrix, new Mat(), svdU, new Mat());
				tx = svdU.get(0, 0)[0];
				ty = svdU.get(1, 0)[0];
			}
			if (tx < 0) {
				tx = -tx;
				ty = -ty;
			}
			// Core.PCACompute(data, mean, eigenvectors);
			this.tangent = new Point(tx, ty);

			this.antiTangent = new Point(ty, -tx);
			this.lxmin = Double.MAX_VALUE;
			this.lxmax = 0;
			this.lymin = Double.MAX_VALUE;
			this.lymax = 0;
			for (Point pt : contour.toArray()) {
				double clx = this.tangent.x * (pt.x - center.x) + this.tangent.y * (pt.y - center.y);
				if (clx < this.lxmin)
					this.lxmin = clx;
				if (clx > this.lxmax)
					this.lxmax = clx;

				double anticlx = this.antiTangent.x * (pt.x - center.x) + this.antiTangent.y * (pt.y - center.y);
				if (anticlx < this.lymin)
					this.lymin = anticlx;
				if (anticlx > this.lymax)
					this.lymax = anticlx;
			}

			this.left = new Point(center.x + tangent.x * lxmin, center.y + tangent.y * lxmin);
			this.right = new Point(center.x + tangent.x * lxmax, center.y + tangent.y * lxmax);

			this.angle = Math.atan2(tangent.y, tangent.x);
			this.isLeaf = isLeaf;
			if (lxmin >= lxmax)
				throw new IllegalStateException();
			if (lymin >= lymax)
				throw new IllegalStateException();
		}

		@Override
		public int compareTo(SuperContour c) {
			return Double.compare(rect.tl().y, c.rect.tl().y);
		}

		public double xLocalOverlap(SuperContour c2) {
			double xmin = (c2.left.x - center.x) * tangent.x + (c2.left.y - center.y) * tangent.y;
			double xmax = (c2.right.x - center.x) * tangent.x + (c2.right.y - center.y) * tangent.y;
			return Math.min(lxmax, xmax) - Math.max(lxmin, xmin);
		}

		public double yLocalOverlap(SuperContour c2) {
			double ymin = (c2.left.x - center.x) * antiTangent.x + (c2.left.y - center.y) * antiTangent.y;
			double ymax = (c2.right.x - center.x) * antiTangent.x + (c2.right.y - center.y) * antiTangent.y;
			return Math.min(lymax, ymax) - Math.max(lymin, ymin);
		}

	}

	public static class Edge implements Comparable<Edge> {

		private final double score;
		private final SuperContour c1;
		private final SuperContour c2;

		public Edge(double score, SuperContour c1, SuperContour c2) {
			this.c1 = c1;
			this.c2 = c2;
			this.score = score;
		}

		@Override
		public int compareTo(Edge edge) {
			return Double.compare(score, edge.getScore());
		}

		private double getScore() {
			return score;
		}

		public SuperContour getC1() {
			return c1;
		}

		public SuperContour getC2() {
			return c2;
		}

	}

	public static class Span {

		private List<SuperContour> contours = new ArrayList<>();

		public void add(SuperContour superContour) {
			contours.add(superContour);
		}

		public List<SuperContour> getContours() {
			return contours;
		}
	}

	public List<Span> assembleContours(List<SuperContour> superContours) {
		Collections.sort(superContours);
		List<Edge> candidateEdges = new ArrayList<>();
		for (int i = 0; i < superContours.size(); i++)
			for (int j = 0; j < i; j++) {
				Edge edge = generateCandidateEdge(superContours.get(i), superContours.get(j));
				if (edge != null)
					candidateEdges.add(edge);
			}
		Collections.sort(candidateEdges);
		for (Edge edge : candidateEdges)
			if (edge.getC1().succ == null && edge.getC2().pred == null) {
				edge.getC1().succ = edge.getC2();
				edge.getC2().pred = edge.getC1();
			}
		List<Span> spans = new ArrayList<>();
		while (!superContours.isEmpty()) {
			SuperContour contour = superContours.get(0);
			int i = 100;
			while (contour.pred != null) {
				if (i < 10)
					System.out.println("pred" + contour.rect + " " + contour.left + " " + contour.right);
				if (i-- < 0)
					throw new IllegalStateException();
				contour = contour.pred;
			}
			Span curSpan = new Span();
			double width = 0.0;
			i = 100;
			while (contour != null) {
				if (i < 10)
					System.out.println("succ " + contour.rect + " " + contour.left + " " + contour.right);
				if (i-- < 0)
					throw new IllegalStateException();
				superContours.remove(contour);
				curSpan.add(contour);
				width += contour.lxmax - contour.lxmin;
				contour = contour.succ;
			}
			double SPAN_MIN_WIDTH = 30;
			if (width > SPAN_MIN_WIDTH)
				spans.add(curSpan);
		}
		return spans;
	}

	private Edge generateCandidateEdge(SuperContour c1, SuperContour c2) {

		if (c1.right.x > c2.left.x) {
			SuperContour tmp = c1;
			c1 = c2;
			c2 = tmp;
		}

		if (c1.right.x > c2.left.x) {
			int result = Double.compare(c1.center.x, c2.center.x);
			if (result == 0)
				result = Double.compare(c1.center.y, c2.center.y);
			if (result > 0) {
				SuperContour tmp = c1;
				c1 = c2;
				c2 = tmp;
			}
			if (c1.center.x > c2.center.x)
				throw new IllegalStateException(c1.center.x + " " + c2.center.x);
			double dist = Math.sqrt(Math.pow(c1.center.x - c2.center.x, 2) + Math.pow(c1.center.y - c2.center.y, 2) / 2);
			double[] overall_tangent = new double[] { c2.center.x - c1.center.x, c2.center.y - c1.center.y };
			// if (dist > 30) {
			// double x_overlap = Math.max(c1.xLocalOverlap(c2), c2.xLocalOverlap(c1));
			// if (x_overlap < -10)
			// return null;
			// double y_overlap = Math.max(c1.yLocalOverlap(c2), c2.yLocalOverlap(c1));
			// if (y_overlap < -10)
			// return null;
			// }

			if (dist > 15 && Math.atan2(Math.abs(overall_tangent[1]), Math.abs(overall_tangent[0])) * 180 / Math.PI > 4)
				return null;
			return new Edge(dist, c1, c2);
		}

		double dist = Math.sqrt(Math.pow(c1.right.x - c2.left.x, 2) + Math.pow(c1.right.y - c2.left.y, 2));

		double[] overall_tangent = new double[] { c2.center.x - c1.center.x, c2.center.y - c1.center.y };
		double overall_angle = Math.atan2(overall_tangent[1], overall_tangent[0]);
		double delta_angle = angle_dist(c1.angle, overall_angle) * (c1.lxmax - c1.lxmin) + angle_dist(c2.angle, overall_angle) * (c2.lxmax - c2.lxmin);

		if (dist > 15 && Math.atan2(Math.abs(overall_tangent[1]), Math.abs(overall_tangent[0])) * 180 / Math.PI > 4)
			return null;
		double score = dist;// + delta_angle * 10;
		return new Edge(score, c1, c2);
	}

	private double angle_dist(double angle_b, double angle_a) {
		double diff = angle_b - angle_a;
		while (diff > Math.PI)
			diff -= 2 * Math.PI;
		while (diff < -Math.PI)
			diff += 2 * Math.PI;
		return Math.abs(diff);
	}

	List<Rect> detectRects(Img binarized, int minArea, int maxArea, double fillRatio) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(binarized.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		Size size = binarized.size();
		List<Rect> result = new ArrayList<>();
		for (MatOfPoint contour : contours) {
			double area = Imgproc.contourArea(contour);
			if (area > minArea && area < maxArea) {
				Rect rect = Imgproc.boundingRect(contour);
				if (rect.tl().x != 0 && rect.tl().y != 0 && rect.br().x != size.width && rect.br().y != size.height)
					if (getFillRatio(contour, rect) > fillRatio)
						result.add(rect);

			}
		}
		Collections.reverse(result);
		return result;
	}

	public double getFillRatio(MatOfPoint contour, Rect rect) {
		Mat mask = Mat.zeros(rect.size(), CvType.CV_8UC1);
		Imgproc.drawContours(mask, Arrays.asList(contour), 0, new Scalar(255), -1, Imgproc.LINE_8, new Mat(), Integer.MAX_VALUE, new Point(-rect.tl().x, -rect.tl().y));
		Mat mat = new Mat();
		Core.findNonZero(mask, mat);
		return mat.rows() / rect.area();
	}

	public List<GSRect> cleanList(List<GSRect> bigRects, List<GSRect> smallRects, double overlapThreshold) {
		smallRects.removeIf(smallRect -> bigRects.stream().anyMatch(bigRect -> smallRect.inclusiveArea(bigRect) > overlapThreshold));
		return Stream.concat(smallRects.stream().filter(smallRect -> bigRects.stream().filter(rect -> rect.isOverlapping(smallRect)).noneMatch(rect -> rect.getInsider(smallRect) == null)), bigRects.stream()).collect(Collectors.toList());
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

	public void putText(String text) {
		Imgproc.putText(getDisplay().getSrc(), text, new Point(getDisplay().width() / 2, 20), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 255, 255), 1);
	}

}
