package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.cv.LinesDetector.Damper;
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

public class LinesDetector3 extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private Damper vpxDamper = new Damper(1);
	private Damper vpyDamper = new Damper(1);

	@Override
	protected void fillGrid(GridPane mainGrid) {
		vpxDamper.pushNewValue(0);
		vpyDamper.pushNewValue(0);
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
					Point vp = (Point) ransac.getBestModel().getParams()[0];
					vpxDamper.pushNewValue(vp.x);
					vpyDamper.pushNewValue(vp.y);
					Point bary = new Point(frame.width() / 2, frame.height() / 2);
					Mat homography = findHomography(new Point(vpxDamper.getMean(), vpyDamper.getMean()), bary, frame.width(), frame.height());
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

	public Point center(Point a, Point b) {
		return new Point((a.x + b.x) / 2, (a.y + b.y) / 2);
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
		if (Math.abs(rotatedVp.x - width / 2) > 1000) {
			A_ = A;
			B_ = B;
			C_ = C;
			D_ = D;
			System.out.println("------------------only rotate----------------------------------");
		} else if (rotatedVp.x >= width / 2) {
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

		//System.out.println("vp : " + vp);
		 System.out.println("rotated vp : " + rotatedVp);
		 System.out.println("Alpha : " + alpha * 180 / Math.PI);
		// System.out.println("A : " + A + " " + A_);
		// System.out.println("B : " + B + " " + B_);
		// System.out.println("C : " + C + " " + C_);
		// System.out.println("D : " + D + " " + D_);

		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(rotate(bary, -alpha, A_, B_, C_, D_)), new MatOfPoint2f(A, B, C, D));
	}

	// private Mat findHomography(Point vp, Point bary, double width, double height) {
	// System.out.println("vpx : " + vp.x);
	// System.out.println("vpy : " + vp.y);
	//
	// double alpha_ = Math.atan2((vp.y - bary.y), (vp.x - bary.x));
	// if (alpha_ < -Math.PI / 2 && alpha_ > -Math.PI)
	// alpha_ = alpha_ + Math.PI;
	// if (alpha_ < Math.PI && alpha_ > Math.PI / 2)
	// alpha_ = alpha_ - Math.PI;
	// double alpha = alpha_;
	//
	// Point A = new Point(width / 2, height / 2);
	// Point B = new Point(width, height / 2);
	// Point C = new Point(width, height);
	// Point D = new Point(width / 2, height);
	//
	// Point A_ = A;
	// Point B_ = new Point(bary.x + Math.cos(alpha) * width / 2, bary.y + Math.sin(alpha) * width / 2);
	// Point D_ = new Point(bary.x - Math.sin(alpha) * height / 2, bary.y + Math.cos(alpha) * height / 2);
	//
	// double a_1 = -1 / ((vp.y - bary.y) / (vp.x - bary.x));
	// double b_1 = B_.y - a_1 * B_.x;
	//
	// double a_2 = (vp.y - D_.y) / (vp.x - D_.x);
	// double b_2 = vp.y - a_2 * vp.x;
	//
	// double tmpx = (b_2 - b_1) / (a_1 - a_2);
	// Point C_ = new Point(tmpx, a_2 * tmpx + b_2);
	//
	// System.out.println("Alpha : " + alpha * 180 / Math.PI);
	// System.out.println("A : " + A + " " + A_);
	// System.out.println("B : " + B + " " + B_);
	// System.out.println("C : " + C + " " + C_);
	// System.out.println("D : " + D + " " + D_);
	//
	// Mat homography = Imgproc.getPerspectiveTransform(new MatOfPoint2f(A_, B_, C_, D_), new MatOfPoint2f(A, B, C, D));
	//
	// MatOfPoint2f results = new MatOfPoint2f();
	// Core.perspectiveTransform(new MatOfPoint2f(new Point(0, 0), bary, new Point(width, height)), results, homography);
	// Point[] targets = results.toArray();
	// System.out.println("Carré : " + Arrays.toString(targets));
	// return homography;
	// }

	public static class Lines {

		private final List<Line> lines = new ArrayList<>();
		private final double mean;

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

		public Ransac<Line> vanishingPointRansac(int width, int height) {

			Function<Collection<Line>, Model<Line>> modelProvider = datas -> {
				Iterator<Line> it = datas.iterator();
				Line line = it.next();
				Line line2 = it.next();
				Point vp = line.intersection(line2);
				return new Model<Line>() {

					@Override
					public double computeError(Line line) {
						if (!Double.isFinite(vp.y) || !Double.isFinite(vp.x))
							return Double.MAX_VALUE;
						// Line transformed = line.perspectivTransform(homography[0]);
						// return transformed.getAngle() * line.size();
						return line.distance(vp);
					}

					@Override
					public double computeGlobalError(Collection<Line> datas) {
						double error = 0;
						double lineL = 0;
						for (Line line : datas) {
							error += Math.pow(computeError(line) * line.size(), 2);
							lineL += Math.pow(line.size(), 2);
						}
						return Math.sqrt(error) / Math.sqrt(lineL);
					}

					@Override
					public Object[] getParams() {
						return new Object[] { vp };
					}

				};
			};

			Ransac<Line> ransac = new Ransac<>(lines, modelProvider, 2, 200, 100, Double.valueOf(Math.floor(lines.size() * 0.4)).intValue());
			ransac.compute(false);
			return ransac;
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
