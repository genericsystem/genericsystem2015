package org.genericsystem.cv;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.genericsystem.cv.utils.Line;
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
				if (lines.size() > 16) {
					lines.draw(frame, new Scalar(0, 0, 255));
					lines = lines.ransacMean();
					lines.draw(frame, new Scalar(0, 255, 0));
					damper.pushNewValue(lines.getMean());
					System.out.println("Ransac angle: " + lines.getMean() / Math.PI * 180);
					binaryView.setImage(Tools.mat2jfxImage(grad.getSrc()));
					frameView.setImage(Tools.mat2jfxImage(frame));

					Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), lines.getMean() / Math.PI * 180, 1);
					Mat rotated = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
					Mat rotatedMasked = new Mat();
					Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
					Mat warpedMask = new Mat();
					Imgproc.warpAffine(mask, warpedMask, matrix, frame.size());
					Imgproc.warpAffine(frame, rotatedMasked, matrix, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
					rotatedMasked.copyTo(rotated, warpedMask);

					lines = Lines.of(lines.rotate(matrix));
					rotatedView.setImage(Tools.mat2jfxImage(rotated));

					Mat dePerspectived = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
					Ransac<Line> ransac = lines.findPerspectiveMatrix(frame.width(), frame.height());
					Mat homography = (Mat) ransac.getBestModel().getParams()[0];
					mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
					Mat maskWarpped = new Mat();
					Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
					Mat tmp = new Mat();
					Imgproc.warpPerspective(rotated, tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
					tmp.copyTo(dePerspectived, maskWarpped);
					lines = new Lines(ransac.getBestDataSet().values());
					lines.draw(dePerspectived, new Scalar(255, 0, 0));
					deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));

				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Exception e) {
				e.printStackTrace();
			}

		}, 0, 33, TimeUnit.MILLISECONDS);

	}

	public static class Lines extends org.genericsystem.cv.utils.Lines {

		public Lines(Mat src) {
			super(src);
		}

		public Lines(Collection<Line> lines) {
			super(lines);
		}

		public static Lines of(Collection<Line> lines) {
			return new Lines(lines);
		}

		public Ransac<Line> findPerspectiveMatrix(int width, int height) {
			Function<Collection<Line>, Model<Line>> modelProvider = datas -> {
				Line line = datas.iterator().next();
				// if (datas.size() > 1)
				// throw new IllegalStateException("" + datas.size());
				double a = (line.getY2() - line.getY1()) / (line.getX2() - line.getX1());
				double b = (line.getY1() + line.getY2() - a * (line.getX1() + line.getX2())) / 2;
				double newy = a * width / 2 + b;

				Mat homography = Imgproc.getPerspectiveTransform(new MatOfPoint2f(new Point[] { new Point(line.getX1(), line.getY1()), new Point(line.getX2(), line.getY2()), new Point(line.getX2(), height / 2), new Point(line.getX1(), height / 2) }),
						new MatOfPoint2f(new Point[] { new Point(line.getX1(), newy), new Point(line.getX2(), newy), new Point(line.getX2(), height / 2), new Point(line.getX1(), height / 2) }));

				return new Model<Line>() {

					@Override
					public double computeError(Line line) {
						return Math.abs(line.perspectivTransform(homography).getAngle());
					}

					@Override
					public double computeGlobalError(List<Line> datas, Collection<Line> consensusData) {
						double error = 0;
						for (Line data : datas)
							error += Math.pow(computeError(data), 2);
						return error / datas.size();
					}

					@Override
					public Object[] getParams() {
						return new Object[] { homography };
					}

				};
			};

			Ransac<Line> ransac = new Ransac<>(lines, modelProvider, 1, 200, 3 * Math.PI / 180, Double.valueOf(Math.floor(lines.size() * 0.6)).intValue());
			// System.out.println("Error max : " + ransac.getBestError());
			// System.out.println("----------------------");
			// for (Line line : lines) {
			// System.out.println("line angle : " + line.getAngle() + " result angle :" + line.perspectivTransform((Mat) ransac.getBestModel().getParams()[0]).getAngle());
			// }
			// System.out.println("------end-------------");

			return ransac;
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

					@Override
					public double computeGlobalError(List<Line> datas, Collection<Line> consensusData) {
						double error = 0;
						for (Line data : consensusData)
							error += Math.pow(computeError(data), 2);
						return error / datas.size();
					}

				};
			};
			return new Lines(new Ransac<>(lines, modelProvider, lines.size() / 8, 100, 25 * Math.PI / 180, lines.size() / 3).getBestDataSet().values());
		}
	}

	public static class Damper {
		private final int maxSize;
		private final ArrayDeque<Double> deque;

		Damper(int maxSize) {
			this.maxSize = maxSize;
			deque = new ArrayDeque<>(maxSize + 1);
		}

		void pushNewValue(double value) {
			deque.push(value);
			if (deque.size() > maxSize)
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
