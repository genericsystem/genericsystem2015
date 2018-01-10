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
import org.genericsystem.cv.lm.LevenbergImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;

import javafx.application.Platform;
import javafx.scene.image.Image;
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

	private SuperFrameImg superFrame;

	@Override
	protected void fillGrid(GridPane mainGrid) {
		superFrame = SuperFrameImg.create(capture);
		Image jfx = superFrame.getDisplay().toJfxImage();
		ImageView view00 = new ImageView(jfx);
		mainGrid.add(view00, 0, 0);
		ImageView view01 = new ImageView(jfx);
		mainGrid.add(view01, 0, 1);
		ImageView view10 = new ImageView(jfx);
		mainGrid.add(view10, 1, 0);
		ImageView view11 = new ImageView(jfx);
		mainGrid.add(view11, 1, 1);

		double[] pp = superFrame.getPrincipalPoint();
		calibrated0 = new AngleCalibrated(0, Math.PI / 2);
		timer.scheduleAtFixedRate(() -> {
			try {

				if (!stabilizedMode) {
					superFrame = SuperFrameImg.create(capture);
				}
				Lines lines = superFrame.detectLines();
				if (textsEnabledMode)
					lines.lines.addAll(TextOrientationLinesDetector.getTextOrientationLines(superFrame));
				if (lines.size() > 4) {
					superFrame.draw(lines, new Scalar(0, 0, 255), 1);

					double[] thetaPhi = new LevenbergImpl<>((line, params) -> new AngleCalibrated(params).distance(line, pp, f), lines.lines, calibrated0.getThetaPhi()).getParams();
					calibrated0 = new AngleCalibrated(thetaPhi);
					AngleCalibrated[] calibratedVps = calibrated0.findOtherVps(lines.lines, pp, f);

					calibratedVps[0].draw(superFrame, lines, pp, f, new Scalar(0, 255, 0), 1);
					calibratedVps[1].draw(superFrame, lines, pp, f, new Scalar(255, 0, 0), 1);

					Image displayImage = superFrame.getDisplay().toJfxImage();
					Image deperspectivedImage = superFrame.dePerspective(calibratedVps, pp, f).toJfxImage();
					Image closed = superFrame.getBinaryClosed30().toJfxImage();
					Image diff = superFrame.getDiffFrame().toJfxImage();

					Platform.runLater(() -> {
						view00.setImage(displayImage);
						view01.setImage(deperspectivedImage);
						view10.setImage(closed);
						view11.setImage(diff);
					});
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

	public static class Lines {

		final List<Line> lines;

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
		final double x1, y1, x2, y2;

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

	static class Circle {
		public Circle(Point center, float radius) {
			this.center = center;
			this.radius = radius;
		}

		Point center;
		float radius;
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
