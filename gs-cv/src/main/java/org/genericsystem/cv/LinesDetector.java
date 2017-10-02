package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Ransac;
import org.genericsystem.layout.Ransac.Model;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class LinesDetector extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		capture.read(frame);

		ImageView canny = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(canny, 0, 0);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 0, 1);
		ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src2, 1, 0);
		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img img = new Img(frame, false);
				Img grad = img.morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();
				Img gray = grad;
				// Img blured = gray.gaussianBlur(new Size(3, 3));

				// MatOfDouble mu = new MatOfDouble();
				// Core.meanStdDev(gray.getSrc(), mu, new MatOfDouble());
				// double median = mu.get(0, 0)[0];
				// double sigma = 0.33;
				// double lower = Math.max(0, (1.0 - sigma) * median);
				// double upper = Math.min(255, (1.0 + sigma) * median);
				// Img edges = gray.canny(lower, upper);
				// Imgproc.dilate(gray.getSrc(), gray.getSrc(), new Mat());

				Mat lines = gray.houghLinesP(1, Math.PI / 180, 100, 100, 10);
				Lines lines_ = new Lines(lines);

				System.out.println("Average angle: " + lines_.getMean() / Math.PI * 180);
				if (lines_.size() > 16) {
					lines_ = lines_.ransacMean();
					System.out.println("Ransac angle: " + lines_.getMean() / Math.PI * 180);
				} else
					System.out.println("Not enough lines : " + lines_.size());

				double angle = 0.;
				for (int i = 0; i < lines.rows(); i++) {
					double[] val = lines.get(i, 0);
					// Imgproc.line(frame, new Point(val[0], val[1]), new Point(val[2], val[3]), new Scalar(0, 0, 255), 1);
					angle += Math.atan2(val[3] - val[1], val[2] - val[0]);
				}
				angle /= lines.rows(); // mean angle, in radians.

				double j = 0;
				double goodAngle = 0;
				double angle_tmp;
				for (int i = 0; i < lines.rows(); i++) {
					double[] val = lines.get(i, 0);
					angle_tmp = Math.atan2(val[3] - val[1], val[2] - val[0]);
					if (Math.abs((angle - angle_tmp) / Math.PI * 180) < 10) {
						Imgproc.line(frame, new Point(val[0], val[1]), new Point(val[2], val[3]), new Scalar(255, 0, 0), 1);
						goodAngle += angle_tmp;
						j++;
					} else
						Imgproc.line(frame, new Point(val[0], val[1]), new Point(val[2], val[3]), new Scalar(0, 0, 255), 1);

				}
				goodAngle /= j; // mean angle, in radians.
				System.out.println("Good angle: " + goodAngle / Math.PI * 180);

				Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), lines_.getMean() / Math.PI * 180, 1);
				Mat rotated = new Mat();
				Imgproc.warpAffine(frame, rotated, matrix, new Size(frame.size().width, frame.size().height));
				double crop = 0.;
				Img croppedImg = new Img(new Mat(rotated, new Rect(Double.valueOf(rotated.width() * crop).intValue(), Double.valueOf(rotated.height() * crop).intValue(), Double.valueOf(rotated.width() * (1 - 2 * crop)).intValue(), Double.valueOf(
						rotated.height() * (1 - 2 * crop)).intValue())));

				canny.setImage(Tools.mat2jfxImage(gray.getSrc()));
				src.setImage(Tools.mat2jfxImage(frame));
				src2.setImage(Tools.mat2jfxImage(croppedImg.getSrc()));

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
				// System.out.println(datas + "  " + meanParam);
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
			Ransac<Line> ransac = new Ransac<Line>(lines, modelProvider, lines.size() / 8, 100, 10 * Math.PI / 180, lines.size() / 3);
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
