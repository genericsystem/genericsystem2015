package org.genericsystem.cv;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
import org.genericsystem.cv.utils.VanishingPointsDetector;
import org.genericsystem.layout.Ransac;
import org.genericsystem.layout.Ransac.Model;
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

public class LinesDetector extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private Damper damper = new Damper(10);

	@Override
	protected void fillGrid(GridPane mainGrid) {
		damper.pushNewValue(0);
		Mat frame = new Mat();
		capture.read(frame);

		ImageView binaryView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(binaryView, 0, 0);
		ImageView frameView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(frameView, 0, 1);
		ImageView rotatedView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(rotatedView, 1, 0);
		ImageView deskiewedView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(deskiewedView, 1, 1);
		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();
				Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 100, 100, 10));
				List<List<Point[]>> lineSegmentsClusters = new ArrayList<>();
				List<Integer> numInliers = new ArrayList<>();
				List<Mat> vps = new ArrayList<>();
				new VanishingPointsDetector(frame.size(), true).multipleVPEstimation(lines.toLinesSegments(), lineSegmentsClusters, numInliers, vps, 1);

				System.out.println("Average angle: " + lines.getMean() / Math.PI * 180);
				if (lines.size() > 16) {
					lines.draw(frame, new Scalar(0, 0, 255));
					lines = lines.ransacMean();
					lines.draw(frame, new Scalar(0, 255, 0));
					damper.pushNewValue(lines.getMean());
					System.out.println("Ransac angle: " + lines.getMean() / Math.PI * 180);
					binaryView.setImage(Tools.mat2jfxImage(grad.getSrc()));
					frameView.setImage(Tools.mat2jfxImage(frame));

					Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), damper.getMean() / Math.PI * 180, 1);
					Mat rotated = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
					Mat rotatedMasked = new Mat();
					Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
					Mat warpedMask = new Mat();
					Imgproc.warpAffine(mask, warpedMask, matrix, frame.size());
					Imgproc.warpAffine(frame, rotatedMasked, matrix, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
					rotatedMasked.copyTo(rotated, warpedMask);
					rotatedView.setImage(Tools.mat2jfxImage(rotated));

					lines = lines.rotate(matrix);
					System.out.println("zzzz" + lines.getMean());

					Mat dePerspectived = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
					Mat homography = lines.findPerspectiveMatrix(frame.width(), frame.height());
					mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
					Mat maskWarpped = new Mat();
					Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
					Mat tmp = new Mat();
					Imgproc.warpPerspective(rotated, tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
					tmp.copyTo(dePerspectived, maskWarpped);
					deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));

				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Exception e) {
				e.printStackTrace();
			}

		}, 0, 33, TimeUnit.MILLISECONDS);

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

		public List<Point[]> toLinesSegments() {
			return lines.stream().map(line -> line.getSegment()).collect(Collectors.toList());
		}

		public Mat findPerspectiveMatrix(int width, int height) {
			Function<Collection<Line>, Model<Line>> modelProvider = datas -> {
				Line line = datas.iterator().next();
				if (datas.size() > 1)
					throw new IllegalStateException("" + datas.size());
				double a = (line.y2 - line.y1) / (line.x2 - line.x1);
				double b = (line.y1 + line.y2 - a * (line.x1 + line.x2)) / 2;
				double newy = a * width / 2 + b;

				Mat homography = Imgproc.getPerspectiveTransform(new MatOfPoint2f(new Point[] { new Point(line.x1, line.y1), new Point(line.x2, line.y2), new Point(line.x2, height / 2), new Point(line.x1, height / 2) }),
						new MatOfPoint2f(new Point[] { new Point(line.x1, newy), new Point(line.x2, newy), new Point(line.x2, height / 2), new Point(line.x1, height / 2) }));

				return new Model<Line>() {

					@Override
					public double computeError(Line line) {
						return Math.abs(line.perspectivTransform(homography).getAngle());
					}

					@Override
					public Object[] getParams() {
						return new Object[] { homography };
					}

				};
			};

			Ransac<Line> ransac = new Ransac<>(lines, modelProvider, 1, 200, 2 * Math.PI / 180, Double.valueOf(Math.floor(lines.size() * 0.6)).intValue());
			ransac.compute(false);
			// System.out.println("Error max : " + ransac.getBestError());
			// System.out.println("----------------------");
			// for (Line line : lines) {
			// System.out.println("line angle : " + line.getAngle() + " result angle :" + line.perspectivTransform((Mat) ransac.getBestModel().getParams()[0]).getAngle());
			// }
			// System.out.println("------end-------------");

			return (Mat) ransac.getBestModel().getParams()[0];
		}

		public Lines rotate(Mat matrix) {
			return new Lines(lines.stream().map(line -> line.transform(matrix)).collect(Collectors.toList()));
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

		public Lines ransacMean() {

			Function<Collection<Line>, Model<Line>> modelProvider = datas -> {
				Lines lines = new Lines(datas);
				double meanParam = lines.getMean();
				// System.out.println(datas + " " + meanParam);
				return new Model<Line>() {

					@Override
					public double computeError(Line data) {
						return Math.abs(data.getAngle() - meanParam);
					}

					@Override
					public Object[] getParams() {
						return new Object[] { meanParam };
					}

				};
			};
			Ransac<Line> ransac = new Ransac<>(lines, modelProvider, lines.size() / 8, 100, 25 * Math.PI / 180, lines.size() / 3);
			ransac.compute();
			return new Lines(ransac.getBestDataSet().values());
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

		public Point[] getSegment() {
			return new Point[] { new Point(x1, y1), new Point(x2, y2) };
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

	public static class Damper {
		private final int maxSize;
		private final ArrayDeque<Double> deque;

		private Damper(int maxSize) {
			this.maxSize = maxSize;
			deque = new ArrayDeque<>(maxSize + 1);
		}

		void pushNewValue(double value) {
			deque.push(value);
			if (deque.size() >= maxSize)
				deque.removeLast();
		}

		public double getMean() {
			double mean = 0;
			for (Double value : deque)
				mean += value;
			mean /= deque.size();
			return mean;
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
