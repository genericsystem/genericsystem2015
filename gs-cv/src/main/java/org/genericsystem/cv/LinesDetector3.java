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
import org.genericsystem.cv.utils.Tools;
import org.genericsystem.layout.Ransac;
import org.genericsystem.layout.Ransac.Model;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
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
	private Damper vpxDamper = new Damper(10);
	private Damper vpyDamper = new Damper(10);

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
				// Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();
				Img grad = new Img(frame, false).canny(60, 180);
				// /Img grad = new Img(frame, false).bilateralFilter(20, 80, 80).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 11, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(11,
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

	private Mat findHomography(Point vp, Point bary, double width, double height) {
		System.out.println("vpx : " + vp.x);
		System.out.println("vpy : " + vp.y);

		MatOfPoint2f targetPoints = new MatOfPoint2f(new Point(width / 2, height / 2), new Point(width, height / 2), new Point(width, height), new Point(width / 2, height));

		double alpha_ = Math.atan2((vp.y - bary.y), (vp.x - bary.x));
		if (alpha_ < -Math.PI / 2 && alpha_ > -Math.PI)
			alpha_ = alpha_ + Math.PI;
		if (alpha_ < Math.PI && alpha_ > Math.PI / 2)
			alpha_ = alpha_ - Math.PI;
		double alpha = alpha_;

		Point A_ = bary;
		Point B_ = new Point(bary.x + Math.cos(alpha) * width / 2, bary.y + Math.sin(alpha) * width / 2);
		Point D_ = new Point(bary.x - Math.sin(alpha) * height / 2, bary.y + Math.cos(alpha) * height / 2);

		double a_1 = -1 / ((vp.y - bary.y) / (vp.x - bary.x));// Math.tan(alpha + Math.PI / 2);
		double b_1 = B_.y - a_1 * B_.x;

		double a_2 = (vp.y - D_.y) / (vp.x - D_.x);
		double b_2 = vp.y - a_2 * vp.x;

		double tmpx = (b_2 - b_1) / (a_1 - a_2);

		Point C_ = new Point(tmpx, a_2 * tmpx + b_2);
		System.out.println("Alpha : " + alpha * 180 / Math.PI);
		System.out.println("A : " + new Point(width / 2, height / 2) + " " + A_);
		System.out.println("B : " + new Point(width, height / 2) + " " + B_);
		System.out.println("C : " + new Point(width, height) + " " + C_);
		System.out.println("D : " + new Point(width / 2, height) + " " + D_);

		Mat homography = Imgproc.getPerspectiveTransform(new MatOfPoint2f(A_, B_, C_, D_), targetPoints);

		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(new MatOfPoint2f(new Point(0, 0), bary, new Point(width, height)), results, homography);
		Point[] targets = results.toArray();
		System.out.println("Carré : " + Arrays.toString(targets));
		return homography;
	}

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
				if (datas.size() > 2)
					throw new IllegalStateException("" + datas.size());
				double a = (line.y2 - line.y1) / (line.x2 - line.x1);
				double b = line.y1 - a * line.x1;

				Line line2 = it.next();

				double a2 = (line2.y2 - line2.y1) / (line2.x2 - line2.x1);
				double b2 = line2.y1 - a * line2.x1;

				double vpx = (b2 - b) / (a - a2);
				double vpy = a * vpx + b;

				return new Model<Line>() {

					@Override
					public double computeError(Line line) {
						if (!Double.isFinite(vpy) || !Double.isFinite(vpx))
							return Double.MAX_VALUE;
						// Line transformed = line.perspectivTransform(homography[0]);
						// return transformed.getAngle() * line.size();

						double a1 = (line.y2 - line.y1) / (line.x2 - line.x1);
						double b1 = line.y1 - a1 * line.x1;
						return Math.abs(a1 * vpx - vpy + b1) / Math.sqrt(1 + Math.pow(a1, 2));
					}

					@Override
					public double computeGlobalError(Collection<Line> datas) {
						double error = 0;
						for (Line line : datas)
							error += Math.pow(computeError(line), 2);
						return Math.sqrt(error) / Math.sqrt(datas.size());
					}

					@Override
					public Object[] getParams() {
						return new Object[] { new Point(vpx, vpy) };
					}

				};
			};

			Ransac<Line> ransac = new Ransac<>(lines, modelProvider, 2, 300, 100, Double.valueOf(Math.floor(lines.size() * 0.5)).intValue());
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
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

}
