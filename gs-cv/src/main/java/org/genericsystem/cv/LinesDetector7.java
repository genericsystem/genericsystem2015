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
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.lm.LMHostImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class LinesDetector7 extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private double[] vp = new double[] { 0, 0 };
	private AngleCalibrated calibrated;

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		capture.read(frame);

		ImageView frameView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(frameView, 0, 0);
		ImageView deskiewedView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(deskiewedView, 0, 1);
		Mat dePerspectived = frame.clone();

		calibrated = new AngleCalibrated(0, Math.PI / 2);
		double[] pp = new double[] { frame.width() / 2, frame.height() / 2 };
		final double f = 6.053 / 0.009;

		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				// int r = 2; // try r=2, 4, or 8
				// double eps = 0.1 * 0.1; // try eps=0.1^2, 0.2^2, 0.4^2
				// eps *= 255 * 255;
				// GuidedFilter gf = Ximgproc.createGuidedFilter(frame, r, eps);
				// gf.filter(frame, frame);

				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(2, 2)).otsu().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
				if (lines.size() > 10) {
					lines.draw(frame, new Scalar(0, 0, 255));
					// lines = lines.filter(line -> distance(vp, line) < 0.4);
					lines.draw(frame, new Scalar(0, 255, 0));
					frameView.setImage(Tools.mat2jfxImage(frame));
					double[] newThetaPhi = new LMHostImpl<>((line, params) -> distance(new AngleCalibrated(params).uncalibrate(pp, f), line), lines.lines, calibrated.getThetaPhi()).getParams();
					calibrated = calibrated.dumpThetaPhi(newThetaPhi, 1);

					vp = calibrated.uncalibrate(pp, f);
					System.out.println("Vanishing point : " + vp);
					Mat homography = findHomography(frame.size(), new AngleCalibrated[] { calibrated, new AngleCalibrated(Math.PI / 2, Math.PI / 2), new AngleCalibrated(Math.PI / 2, 0) }, pp, f);
					Imgproc.warpPerspective(frame, dePerspectived, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_CONSTANT, Scalar.all(0));
					deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));
				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Throwable e) {
				e.printStackTrace();
			}

		}, 30, 10, TimeUnit.MILLISECONDS);

	}

	private double distance(double[] vp, Line line) {
		double a = line.y1 - line.y2;
		double b = line.x2 - line.x1;
		double c = line.y1 * line.x2 - line.x1 * line.y2;
		double norm = Math.sqrt(a * a + b * b + c * c);
		double n0 = -b / norm;
		double n1 = a / norm;
		double nNorm = Math.sqrt(n0 * n0 + n1 * n1);
		double[] midPoint = getMiLine(line);
		double r0, r1;
		r0 = vp[1] * midPoint[2] - midPoint[1];
		r1 = midPoint[0] - vp[0] * midPoint[2];
		double rNorm = Math.sqrt(r0 * r0 + r1 * r1);
		double num = (r0 * n0 + r1 * n1);
		if (num < 0)
			num = -num;
		double d = 0;
		if (nNorm != 0 && rNorm != 0)
			d = num / (nNorm * rNorm);
		return d < 0.4 ? d : 0.4;
	}

	private double[] getNormalizedLine(Line line) {
		double a = line.y1 - line.y2;
		double b = line.x2 - line.x1;
		double c = line.y1 * line.x2 - line.x1 * line.y2;
		double norm = Math.sqrt(a * a + b * b + c * c);
		return new double[] { a / norm, b / norm, c / norm };
	}

	private double[] getMiLine(Line line) {
		return new double[] { (line.x1 + line.x2) / 2, (line.y1 + line.y2) / 2, 1d };
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
		return uncalibrate(cross(a, b));
	}

	static double[] uncalibrate(double[] a) {
		return new double[] { a[0] / a[2], a[1] / a[2], 1 };
	}

	static double[] cross(double[] a, double b[]) {
		return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
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

		public void draw(Mat frame, Scalar color) {
			lines.forEach(line -> line.draw(frame, color));
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

		public void draw(Mat frame, Scalar color) {
			Imgproc.line(frame, new Point(x1, y1), new Point(x2, y2), color, 1);
		}

		public double geta() {
			return (y2 - y1) / (x2 - x1);
		}

		public double getb() {
			return y1 - geta() * x1;
		}

		public double distance(Point p) {
			return Math.abs(geta() * p.x - p.y + getb()) / Math.sqrt(1 + Math.pow(geta(), 2));
		}

		public Point intersection(double a, double b) {
			double x = (b - getb()) / (geta() - a);
			double y = a * x + b;
			return new Point(x, y);
		}

		public Point intersection(Line line) {
			double x = (line.getb() - getb()) / (geta() - line.geta());
			double y = geta() * x + getb();
			return new Point(x, y);
		}

		public Point intersection(double verticalLinex) {
			double x = verticalLinex;
			double y = geta() * x + getb();
			return new Point(x, y);
		}
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

}