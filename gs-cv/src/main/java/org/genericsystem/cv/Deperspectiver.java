package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.lm.LMHostImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
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
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class Deperspectiver extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();;

	private AngleCalibrated calibrated0;

	private final double f = 6.053 / 0.009;
	private boolean stabilizedMode = false;
	private boolean textsEnabledMode = false;
	private Lines lines;
	private SuperFrameImg superFrame;

	@Override
	protected void fillGrid(GridPane mainGrid) {
		superFrame = SuperFrameImg.create(capture);

		ImageView view00 = new ImageView(superFrame.getDisplay().toJfxImage());
		mainGrid.add(view00, 0, 0);
		ImageView view01 = new ImageView(superFrame.getDisplay().toJfxImage());
		mainGrid.add(view01, 0, 1);
		ImageView view10 = new ImageView(superFrame.getDisplay().toJfxImage());
		mainGrid.add(view10, 1, 0);
		ImageView view11 = new ImageView(superFrame.getDisplay().toJfxImage());
		mainGrid.add(view11, 1, 1);

		double[] pp = superFrame.getPrincipalPoint();
		calibrated0 = new AngleCalibrated(0, Math.PI / 2);
		timer.scheduleAtFixedRate(() -> {
			try {

				if (!stabilizedMode) {
					superFrame = SuperFrameImg.create(capture);
				}

				List<Line> addedLines = null;
				if (textsEnabledMode) {
					List<Circle> circles = detectCircles(superFrame, 30, 100);
					Collection<Circle> selectedCircles = selectRandomCirles(circles, 20);
					addedLines = new ArrayList<>();
					for (Circle circle : selectedCircles) {
						Img circledImg = getCircledImg(superFrame.getFrame().getSrc(), (int) circle.radius, circle.center);
						double angle = getBestAngle(circledImg, 42, 12, 5, 180, null) / 180 * Math.PI;
						addedLines.add(buildLine(circle.center, angle, circle.radius));
						Imgproc.circle(superFrame.getDisplay().getSrc(), circle.center, (int) circle.radius, new Scalar(0, 255, 0), 1);
					}
				}

				Img grad = superFrame.getBinaryClosed10().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 10, 3));
				Img grad2 = superFrame.getBinaryClosed20().morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				lines.lines.addAll(new Lines(grad2.houghLinesP(1, Math.PI / 180, 10, 20, 6)).lines);

				if (addedLines != null)
					lines.lines.addAll(addedLines);
				if (lines.size() > 4) {
					lines.draw(superFrame.getDisplay().getSrc(), new Scalar(0, 0, 255), 1);

					double[] thetaPhi = new LMHostImpl<>((line, params) -> distance(new AngleCalibrated(params).uncalibrate(pp, f), line), lines.lines, calibrated0.getThetaPhi()).getParams();
					calibrated0 = calibrated0.dumpThetaPhi(thetaPhi, 1);

					AngleCalibrated[] result = findOtherVps(calibrated0, lines, pp, f);

					double[] uncalibrate0 = result[0].uncalibrate(pp, f);
					Lines horizontals = lines.filter(line -> distance(uncalibrate0, line) < 0.40);
					horizontals.draw(superFrame.getDisplay().getSrc(), new Scalar(0, 255, 0), 1);

					double[] uncalibrate1 = result[1].uncalibrate(pp, f);
					Lines verticals = lines.filter(line -> distance(uncalibrate1, line) < 0.40);
					verticals.draw(superFrame.getDisplay().getSrc(), new Scalar(255, 0, 0), 1);

					view00.setImage(superFrame.getDisplay().toJfxImage());
					Mat homography = findHomography(superFrame.size(), result, pp, f);
					Mat deperspectived = new Mat();
					Imgproc.warpPerspective(superFrame.getFrame().getSrc(), deperspectived, homography, superFrame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
					view01.setImage(Tools.mat2jfxImage(deperspectived));
					view10.setImage(superFrame.getBinaryClosed10().toJfxImage());
					view11.setImage(superFrame.getDiffFrame().toJfxImage());
				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Throwable e) {
				e.printStackTrace();
			}

		}, 30, 30, TimeUnit.MILLISECONDS);

	}

	@Override
	protected void onT() {
		textsEnabledMode = !textsEnabledMode;
	}

	public static AngleCalibrated[] findOtherVps(AngleCalibrated calibrated0, Lines lines, double[] pp, double f) {
		System.out.println(Arrays.toString(calibrated0.getCalibratexyz()));
		AngleCalibrated[] result = new AngleCalibrated[] { null, null, null };
		double bestError = Double.MAX_VALUE;
		double bestAngle = 0;
		for (double angle = 0; angle < 360 / 180 * Math.PI; angle += 1 * Math.PI / 180) {
			double error = 0;
			AngleCalibrated calibratexy = calibrated0.getOrthoFromAngle(angle);
			AngleCalibrated calibratez = calibrated0.getOrthoFromVps(calibratexy);
			if (calibratexy.getPhi() < calibratez.getPhi()) {
				AngleCalibrated tmp = calibratexy;
				calibratexy = calibratez;
				calibratez = tmp;
			}
			double[] uncalibrate = calibratexy.uncalibrate(pp, f);
			for (Line line : lines.lines)
				error += distance(uncalibrate, line);
			// System.out.println(error);
			if (error < bestError) {
				bestError = error;
				result[0] = calibrated0;
				result[1] = calibratexy;
				result[2] = calibratez;
				bestAngle = angle;
			}
		}
		System.out.println("Best angle = " + bestAngle * 180 / Math.PI);

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

	public static Mat findHomography(Size size, AngleCalibrated[] calibrateds, double[] pp, double f) {

		double[][] vps = new double[][] { calibrateds[0].getCalibratexyz(), calibrateds[1].getCalibratexyz(), calibrateds[2].getCalibratexyz() };
		// System.out.println("vps : " + Arrays.deepToString(vps));

		double[][] vps2D = getVp2DFromVps(vps, pp, f);
		System.out.println("vps2D : " + Arrays.deepToString(vps2D));

		System.out.println("vp1 " + calibrateds[0]);
		System.out.println("vp2 " + calibrateds[1]);
		System.out.println("vp3 " + calibrateds[2]);

		double theta = calibrateds[0].getTheta();
		double theta2 = calibrateds[1].getTheta();
		double x = size.width / 6;

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

	private static double distance(double[] vp, Line line) {
		double dy = line.y1 - line.y2;
		double dx = line.x2 - line.x1;
		double dz = line.y1 * line.x2 - line.x1 * line.y2;
		double norm = Math.sqrt(dy * dy + dx * dx + dz * dz);
		double n0 = -dx / norm;
		double n1 = dy / norm;
		double nNorm = Math.sqrt(n0 * n0 + n1 * n1);
		double[] midPoint = new double[] { (line.x1 + line.x2) / 2, (line.y1 + line.y2) / 2, 1d };
		double r0 = vp[1] * midPoint[2] - midPoint[1];
		double r1 = midPoint[0] - vp[0] * midPoint[2];
		double rNorm = Math.sqrt(r0 * r0 + r1 * r1);
		double num = r0 * n0 + r1 * n1;
		if (num < 0)
			num = -num;
		double d = 0;
		if (nNorm != 0 && rNorm != 0)
			d = num / (nNorm * rNorm);
		return d < 0.4 ? d : 0.4;
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

	public Point[] rotate(Point bary, double alpha, Point... p) {
		Mat matrix = Imgproc.getRotationMatrix2D(bary, alpha / Math.PI * 180, 1);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.transform(new MatOfPoint2f(p), results, matrix);
		return results.toArray();
	}

	public static class Lines {

		private final List<Line> lines;

		public Lines(Mat src) {
			lines = new ArrayList<>();
			for (int i = 0; i < src.rows(); i++) {
				double[] val = src.get(i, 0);
				Line line = new Line(val[0], val[1], val[2], val[3]);
				lines.add(line);
			}
		}

		public Lines filter(Predicate<Line> predicate) {
			return new Lines(lines.stream().filter(predicate).collect(Collectors.toList()));
		}

		public Lines reduce(int max) {
			if (lines.size() <= max)
				return this;

			Set<Line> newLines = new HashSet<>();
			while (newLines.size() < max)
				newLines.add(lines.get((int) (Math.random() * size())));
			return new Lines((newLines));
		}

		public Lines(Collection<Line> lines) {
			this.lines = new ArrayList<>(lines);
		}

		public Lines rotate(Mat matrix) {
			return new Lines(lines.stream().map(line -> line.transform(matrix)).collect(Collectors.toList()));
		}

		public Lines perspectivTransform(Mat matrix) {
			return new Lines(lines.stream().map(line -> line.perspectivTransform(matrix)).collect(Collectors.toList()));
		}

		public void draw(Mat frame, Scalar color, int thickness) {
			lines.forEach(line -> line.draw(frame, color, thickness));
		}

		public int size() {
			return lines.size();
		}
	}

	public static class Line {
		private final double x1, y1, x2, y2;

		public Line(Point p1, Point p2) {
			this(p1.x, p1.y, p2.x, p2.y);
		}

		public Line(double x1, double y1, double x2, double y2) {
			this.x1 = x1;
			this.x2 = x2;
			this.y1 = y1;
			this.y2 = y2;
		}

		public double size() {
			return Math.sqrt(Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2));
		}

		public Line transform(Mat rotationMatrix) {
			MatOfPoint2f results = new MatOfPoint2f();
			Core.transform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2))), results, rotationMatrix);
			Point[] targets = results.toArray();
			return new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		}

		public Line perspectivTransform(Mat homography) {
			MatOfPoint2f results = new MatOfPoint2f();
			Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2))), results, homography);
			Point[] targets = results.toArray();
			return new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		}

		public void draw(Mat frame, Scalar color, int thickness) {
			Imgproc.line(frame, new Point(x1, y1), new Point(x2, y2), color, thickness);
		}

	}

	private Collection<Circle> selectRandomCirles(List<Circle> circles, int circlesNumber) {
		if (circles.size() <= circlesNumber)
			return circles;
		Set<Circle> result = new HashSet<>();
		while (result.size() < circlesNumber)
			result.add(circles.get((int) (Math.random() * circles.size())));
		return result;
	}

	private List<Circle> detectCircles(SuperFrameImg superFrame, int minRadius, int maxRadius) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(superFrame.getDiffFrame().getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		List<Circle> circles = new ArrayList<>();
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > 50) {
				float[] radius = new float[1];
				Point center = new Point();
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				Imgproc.minEnclosingCircle(contour2F, center, radius);
				if (radius[0] > minRadius && radius[0] < maxRadius && center.x > radius[0] && center.y > radius[0] && ((center.x + radius[0]) < superFrame.width()) && ((center.y + radius[0]) < superFrame.height())) {
					circles.add(new Circle(center, radius[0]));
					// Imgproc.circle(frame, center, (int) radius[0], new Scalar(0, 0, 255));
				}
				// Imgproc.drawContours(superFrame.getDisplay().getSrc(), Arrays.asList(contour), 0, new Scalar(0, 0, 255), 1);
			}
		}
		return circles;
	}

	private static class Circle {
		public Circle(Point center, float radius) {
			this.center = center;
			this.radius = radius;
		}

		Point center;
		float radius;
	}

	public Img getCircledImg(Mat frame, int radius, Point center) {
		Mat mask = new Mat(new Size(radius * 2, radius * 2), CvType.CV_8UC1, new Scalar(0));
		Imgproc.circle(mask, new Point(radius, radius), radius, new Scalar(255), -1);
		Rect rect = new Rect(new Point(center.x - radius, center.y - radius), new Point(center.x + radius, center.y + radius));
		Mat roi = new Img(new Mat(frame, rect), true).bilateralFilter().adaptativeGaussianInvThreshold(3, 3).getSrc();
		Mat circled = new Mat();
		roi.copyTo(circled, mask);
		Img circledImg = new Img(circled, false);
		return circledImg;
	}

	public Line buildLine(Point center, double angle, double size) {
		double x1 = center.x - Math.sin(angle) * size;
		double y1 = center.y + Math.cos(angle) * size;
		double x2 = center.x + Math.sin(angle) * size;
		double y2 = center.y - Math.cos(angle) * size;
		return new Line(new Point(x1, y1), new Point(x2, y2));
	}

	public double score(Img circled, double angle, int filterSize, double threshold) {
		Mat M = Imgproc.getRotationMatrix2D(new Point(circled.width() / 2, circled.width() / 2), angle, 1);
		Mat rotated = new Mat();
		Imgproc.warpAffine(circled.getSrc(), rotated, M, new Size(circled.width(), circled.width()));
		Img binarized = new Img(rotated, false).directionalFilter(filterSize).thresHold(threshold, 255, Imgproc.THRESH_BINARY);
		Mat result = new Mat();
		Core.reduce(binarized.getSrc(), result, 1, Core.REDUCE_SUM, CvType.CV_64F);
		Core.reduce(result, result, 0, Core.REDUCE_SUM, CvType.CV_64F);
		return result.get(0, 0)[0];
	}

	public double getBestAngle(Img circledImg, int absMinMax, double step, int filterSize, double threshold, Img[] binarized) {
		double maxScore = 0;
		double bestAngle = -1;
		if (binarized != null)
			binarized[0] = new Img(new Mat(new Size(2 * absMinMax * 10, 200), CvType.CV_8UC1, new Scalar(0)), false);
		List<double[]> results = new ArrayList<>();
		for (double angle = -absMinMax; angle <= absMinMax; angle += step) {
			double score = score(circledImg, angle, filterSize, threshold);
			if (angle != 0 && score > maxScore) {
				maxScore = score;
				bestAngle = angle;
			}
			if (angle != 0)
				results.add(new double[] { angle, score });
			// System.out.println(score);
			if (binarized != null)
				new Line((absMinMax + angle) * 10, 0, (absMinMax + angle) * 10, score / 1000).draw(binarized[0].getSrc(), new Scalar(255, 0, 0), 1);
		}
		BiFunction<Double, double[], Double> f = (x, params) -> params[0] * x * x * x * x + params[1] * x * x * x + params[2] * x * x + params[3] * x + params[4];
		BiFunction<double[], double[], Double> e = (xy, params) -> f.apply(xy[0], params) - xy[1];
		double[] result = new LMHostImpl<>(e, results, new double[] { 1, 1, 1, 1, 1 }).getParams();
		Point point = null;
		double polynomAngle = 0.0;
		double max = 0.0;
		for (double angle = -absMinMax; angle <= absMinMax; angle++) {
			Point oldPoint = point;
			double score = f.apply(angle, result);
			point = new Point((absMinMax + angle) * 10, score / 1000);
			if (score > max) {
				max = score;
				polynomAngle = angle;
			}
			if (binarized != null && oldPoint != null)
				new Line(oldPoint, point).draw(binarized[0].getSrc(), new Scalar(255, 0, 0), 1);
		}
		if (binarized != null) {
			Imgproc.circle(binarized[0].getSrc(), new Point((absMinMax + polynomAngle) * 10, max / 1000), 10, new Scalar(255, 255, 0), 3);
			// new Line(new Point((absMinMax + bestAngle) * 10, maxScore / 1000), new Point((absMinMax + bestAngle) * 10, 0)).draw(binarized[0].getSrc(), new Scalar(255, 255, 0), 3);
		}
		// System.out.println(Arrays.toString(result));

		return polynomAngle;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

	@Override
	protected void onSpace() {
		stabilizedMode = !stabilizedMode;
	}

}
