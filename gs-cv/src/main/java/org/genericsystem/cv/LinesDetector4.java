package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.cv.lm.LMHostImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Ransac;
import org.genericsystem.cv.utils.Ransac.Model;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.CvType;
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

public class LinesDetector4 extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private Point vp = new Point(0, 0);

	// private Damper vpxDamper = new Damper(1);
	// private Damper vpyDamper = new Damper(1);

	@Override
	protected void fillGrid(GridPane mainGrid) {
		// vpxDamper.pushNewValue(0);
		// vpyDamper.pushNewValue(0);
		Mat frame = new Mat();
		capture.read(frame);

		ImageView frameView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(frameView, 0, 0);
		ImageView deskiewedView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(deskiewedView, 0, 1);
		Mat dePerspectived = frame.clone();
		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();
				// Img grad = new Img(frame, false).canny(60, 180);
				// Img grad = new Img(frame, false).bilateralFilter(20, 80, 80).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 11, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11,
				// 3));
				Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
				System.out.println("Average angle: " + lines.getMean() / Math.PI * 180);
				if (lines.size() > 10) {
					lines.draw(frame, new Scalar(0, 0, 255));

					frameView.setImage(Tools.mat2jfxImage(frame));
					// Mat dePerspectived = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
					Ransac<Line> ransac = lines.vanishingPointRansac(frame.width(), frame.height());
					Mat vp_mat = (Mat) ransac.getBestModel().getParams()[0];
					Mat uncalibrate = uncalibrate(vp_mat);
					vp = new Point(uncalibrate.get(0, 0)[0], uncalibrate.get(1, 0)[0]);
					System.out.println("Vanishing point : " + vp);
					// vpxDamper.pushNewValue(vp.x);
					// vpyDamper.pushNewValue(vp.y);
					Point bary = new Point(frame.width() / 2, frame.height() / 2);
					Mat homography = findHomography(new Point(vp.x, vp.y), bary, frame.width(), frame.height());
					lines = new Lines(ransac.getBestDataSet().values()).perspectivTransform(homography);

					Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
					Mat maskWarpped = new Mat();
					Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
					Mat tmp = new Mat();
					Imgproc.warpPerspective(frame, tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
					tmp.copyTo(dePerspectived, maskWarpped);
					lines.draw(dePerspectived, new Scalar(0, 255, 0));
					deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));

				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Throwable e) {
				e.printStackTrace();
			}

		}, 33, 250, TimeUnit.MILLISECONDS);

	}

	public Mat uncalibrate(Mat calibrated) {
		Mat uncalibrate = new Mat(3, 1, CvType.CV_32F);
		Core.gemm(Lines.K, calibrated, 1, new Mat(), 0, uncalibrate);
		if (uncalibrate.get(2, 0)[0] != 0) {
			uncalibrate.put(0, 0, new float[] { Double.valueOf(uncalibrate.get(0, 0)[0] / uncalibrate.get(2, 0)[0]).floatValue() });
			uncalibrate.put(1, 0, new float[] { Double.valueOf(uncalibrate.get(1, 0)[0] / uncalibrate.get(2, 0)[0]).floatValue() });
			uncalibrate.put(2, 0, new float[] { 1 });
		}
		return uncalibrate;
	}

	public void print(Mat m) {
		for (int row = 0; row < m.rows(); row++) {
			System.out.print("(");
			for (int col = 0; col < m.cols() - 1; col++) {
				System.out.print(m.get(row, col)[0] + ",");
			}
			System.out.println(m.get(row, m.cols() - 1)[0] + ")");
		}
		System.out.println("---------------");

	}

	public Point[] rotate(Point bary, double alpha, Point... p) {
		Mat matrix = Imgproc.getRotationMatrix2D(bary, alpha / Math.PI * 180, 1);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.transform(new MatOfPoint2f(p), results, matrix);
		return results.toArray();
	}

	private Mat findHomography(Point vp, Point bary, double width, double height) {
		double alpha_ = Math.atan2((vp.y - bary.y), (vp.x - bary.x));
		if (alpha_ < -Math.PI / 2 && alpha_ > -Math.PI)
			alpha_ = alpha_ + Math.PI;
		if (alpha_ < Math.PI && alpha_ > Math.PI / 2)
			alpha_ = alpha_ - Math.PI;
		double alpha = alpha_;

		Point rotatedVp = rotate(bary, alpha, vp)[0];

		Point A = new Point(0, 0);
		Point B = new Point(width, 0);
		Point C = new Point(width, height);
		Point D = new Point(0, height);

		Point AB2 = new Point(width / 2, 0);
		Point CD2 = new Point(width / 2, height);

		Point A_, B_, C_, D_;
		if (rotatedVp.x >= width / 2) {
			A_ = new Line(AB2, rotatedVp).intersection(0);
			D_ = new Line(CD2, rotatedVp).intersection(0);
			C_ = new Line(A_, bary).intersection(new Line(CD2, rotatedVp));
			B_ = new Line(D_, bary).intersection(new Line(AB2, rotatedVp));
		} else {
			B_ = new Line(AB2, rotatedVp).intersection(width);
			C_ = new Line(CD2, rotatedVp).intersection(width);
			A_ = new Line(C_, bary).intersection(new Line(AB2, rotatedVp));
			D_ = new Line(B_, bary).intersection(new Line(CD2, rotatedVp));
		}

		System.out.println("vp : " + vp);
		System.out.println("rotated vp : " + rotatedVp);
		System.out.println("Alpha : " + alpha * 180 / Math.PI);
		// System.out.println("A : " + A + " " + A_);
		// System.out.println("B : " + B + " " + B_);
		// System.out.println("C : " + C + " " + C_);
		// System.out.println("D : " + D + " " + D_);

		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(rotate(bary, -alpha, A_, B_, C_, D_)), new MatOfPoint2f(A, B, C, D));
	}

	public static class Lines {

		private final List<Line> lines = new ArrayList<>();
		private final double mean;
		private static Mat K;

		public Lines(Mat src) {
			double mean = 0;
			for (int i = 0; i < src.rows(); i++) {
				double[] val = src.get(i, 0);
				Line line = new Line(val[0], val[1], val[2], val[3]);
				lines.add(line);
				mean += line.getAngle();
			}
			this.mean = mean / src.rows();
		}

		private Mat getLineMat(Line line) {
			Mat a = new Mat(3, 1, CvType.CV_32F);
			Mat b = new Mat(3, 1, CvType.CV_32F);
			a.put(0, 0, new float[] { Double.valueOf(line.x1).floatValue() });
			a.put(1, 0, new float[] { Double.valueOf(line.y1).floatValue() });
			a.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			b.put(0, 0, new float[] { Double.valueOf(line.x2).floatValue() });
			b.put(1, 0, new float[] { Double.valueOf(line.y2).floatValue() });
			b.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			// Mat an = new Mat(3, 1, CvType.CV_32F);
			// Mat bn = new Mat(3, 1, CvType.CV_32F);
			// Core.gemm(K.inv(), a, 1, new Mat(), 0, an);
			// Core.gemm(K.inv(), b, 1, new Mat(), 0, bn);
			Mat li = a.cross(b);
			Core.normalize(li, li);
			return li;
		}

		private Mat getLineMiMat(Line line) {
			Mat a = new Mat(3, 1, CvType.CV_32F);
			Mat b = new Mat(3, 1, CvType.CV_32F);
			a.put(0, 0, new float[] { Double.valueOf(line.x1).floatValue() });
			a.put(1, 0, new float[] { Double.valueOf(line.y1).floatValue() });
			a.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			b.put(0, 0, new float[] { Double.valueOf(line.x2).floatValue() });
			b.put(1, 0, new float[] { Double.valueOf(line.y2).floatValue() });
			b.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			Mat c = new Mat(3, 1, CvType.CV_32F);
			Core.addWeighted(a, 0.5, b, 0.5, 0, c);
			return c;
		}

		public Ransac<Line> vanishingPointRansac(int width, int height) {

			int minimal_sample_set_dimension = 2;
			double maxError = (float) 0.01623 * 2;
			if (K == null) {
				K = new Mat(3, 3, CvType.CV_32F, new Scalar(0));
				K.put(0, 0, new float[] { width });
				K.put(0, 2, new float[] { width / 2 });
				K.put(1, 1, new float[] { height });
				K.put(1, 2, new float[] { height / 2 });
				K.put(2, 2, new float[] { 1 });
			}
			Mat[] vp = new Mat[1];
			Function<Collection<Line>, Model<Line>> modelProvider = datas -> {

				if (datas.size() == minimal_sample_set_dimension) {
					Iterator<Line> it = datas.iterator();
					vp[0] = getLineMat(it.next()).cross(getLineMat(it.next()));
					// System.out.println("Vanishing point : " + vp[0].get(0, 0)[0] + "," + vp[0].get(1, 0)[0] + "," + vp[0].get(2, 0)[0]);
					Core.gemm(K.inv(), vp[0], 1, new Mat(), 0, vp[0]);
					Core.normalize(vp[0], vp[0]);
					// System.out.println("Vanishing point calibrated : " + vp[0].get(0, 0)[0] + "," + vp[0].get(1, 0)[0] + "," + vp[0].get(2, 0)[0]);
				} else {

					// The starting point is the provided vp which is already calibrated

					// Convert to spherical coordinates to move on the sphere surface (restricted to r=1)
					double x = vp[0].get(0, 0)[0];
					double y = vp[0].get(1, 0)[0];
					double z = vp[0].get(2, 0)[0];
					double r = Core.norm(vp[0]);
					double theta = Math.acos(z / r);
					double phi = Math.atan2(y, x);
					System.out.println("Initial Cal.VP (x,y,z) = (" + x + "," + y + "," + z + ")");

					System.out.println("Initial Cal.VP (Spherical) = (" + theta + "," + phi + "," + r + ")");

					BiFunction<Line, double[], Double> evaluateNieto = (line, params) -> {

						Mat vn = new Mat(3, 1, CvType.CV_32F);
						vn.put(0, 0, new double[] { Math.cos(params[1]) * Math.sin(params[0]) });
						vn.put(1, 0, new double[] { Math.sin(params[1]) * Math.sin(params[0]) });
						vn.put(2, 0, new double[] { Math.cos(params[0]) });
						Core.gemm(K, vn, 1, new Mat(), 0, vn);
						if (vn.get(2, 0)[0] != 0) {
							vn.put(0, 0, new float[] { Double.valueOf(vn.get(0, 0)[0] / vn.get(2, 0)[0]).floatValue() });
							vn.put(1, 0, new float[] { Double.valueOf(vn.get(1, 0)[0] / vn.get(2, 0)[0]).floatValue() });
							vn.put(2, 0, new float[] { 1 });
						}

						return distanceNieto(vn, line);

					};

					LMHostImpl<Line> fitHost = new LMHostImpl<>(evaluateNieto, new ArrayList<>(datas), new double[] { theta, phi });
					double[] params = fitHost.getParms();

					// Store into vp
					// 1) From spherical to cartesian
					theta = params[0];
					phi = params[1];
					x = r * Math.cos(phi) * Math.sin(theta);
					y = r * Math.sin(phi) * Math.sin(theta);
					z = r * Math.cos(theta);
					System.out.println("Converged Cal.VP (x,y,z) = (" + x + "," + y + "," + z + ")");
					System.out.println("Converged Cal.VP (Spherical) = " + "(" + theta + "," + phi + "," + r + ")");

					vp[0].put(0, 0, new float[] { Double.valueOf(x).floatValue() });
					vp[0].put(1, 0, new float[] { Double.valueOf(y).floatValue() });
					vp[0].put(2, 0, new float[] { Double.valueOf(z).floatValue() });

				}

				return new Model<Line>() {

					@Override
					public double computeError(Line line) {

						// The vp arrives here calibrated, need to uncalibrate (check it anyway)
						Mat vn = new Mat(3, 1, CvType.CV_32F);
						if (Math.abs(Core.norm(vp[0]) - 1) < 0.001) {
							Core.gemm(K, vp[0], 1, new Mat(), 0, vn);
							if (vn.get(2, 0)[0] != 0) {
								vn.put(0, 0, new float[] { Double.valueOf(vn.get(0, 0)[0] / vn.get(2, 0)[0]).floatValue() });
								vn.put(1, 0, new float[] { Double.valueOf(vn.get(1, 0)[0] / vn.get(2, 0)[0]).floatValue() });
								vn.put(2, 0, new float[] { 1 });
							}
						}
						double di = distanceNieto(vn, line);
						return di * di;

					}

					@Override
					public double computeGlobalError(List<Line> datas, Collection<Line> consensusDatas) {
						double globalError = 0;
						for (Line line : consensusDatas) {
							double error = computeError(line);
							if (error > maxError)
								error = maxError;
							globalError += error;
						}
						globalError = globalError / datas.size();
						return globalError;
					}

					@Override
					public Object[] getParams() {
						return new Object[] { vp[0] };
					}

				};
			};
			return new Ransac<>(lines, modelProvider, minimal_sample_set_dimension, 100, maxError, Double.valueOf(Math.floor(lines.size() * 0.5)).intValue());
		}

		private double distanceNieto(Mat vp, Line line) {

			Mat lineSegment = getLineMat(line);
			double n0 = -lineSegment.get(1, 0)[0];
			double n1 = lineSegment.get(0, 0)[0];
			double nNorm = Math.sqrt(n0 * n0 + n1 * n1);

			// Mid point
			Mat midPoint = getLineMiMat(line);
			double c0 = midPoint.get(0, 0)[0];
			double c1 = midPoint.get(1, 0)[0];
			double c2 = midPoint.get(2, 0)[0];

			// Vanishing point (uncalibrated)
			double v0 = vp.get(0, 0)[0];
			double v1 = vp.get(1, 0)[0];
			double v2 = vp.get(2, 0)[0];

			double r0, r1;
			r0 = v1 * c2 - v2 * c1;
			r1 = v2 * c0 - v0 * c2;
			double rNorm = Math.sqrt(r0 * r0 + r1 * r1);

			double num = (r0 * n0 + r1 * n1);
			if (num < 0)
				num = -num;

			double d = 0;
			if (nNorm != 0 && rNorm != 0)
				d = num / (nNorm * rNorm);

			// d *= line.size();

			return d;
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

		public Lines(Collection<Line> lines) {
			double mean = 0;
			for (Line line : lines) {
				this.lines.add(line);
				mean += line.getAngle();
			}
			this.mean = mean / lines.size();

		}

		public int size() {
			return lines.size();
		}

		public double getMean() {
			return mean;
		}

	}

	public static class Line {
		private final double x1, y1, x2, y2, angle;

		public Line(Point p1, Point p2) {
			this(p1.x, p1.y, p2.x, p2.y);
		}

		public Line(double x1, double y1, double x2, double y2) {
			this.x1 = x1;
			this.x2 = x2;
			this.y1 = y1;
			this.y2 = y2;
			this.angle = Math.atan2(y2 - y1, x2 - x1);
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

		@Override
		public String toString() {
			return "Line : " + angle;
		}

		public double getAngle() {
			return angle;
		}

		public double geta() {
			return (y2 - y1) / (x2 - x1);
		}

		public double getOrthoa() {
			return (x2 - x1) / (y1 - y2);
		}

		public double getOrthob(Point p) {
			return p.y - getOrthoa() * p.x;
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
