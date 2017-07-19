package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.FeatureDetector;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class CamLayoutAnalyzer extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private final ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

	@Override
	protected void fillGrid(GridPane mainGrid) {
		FeatureDetector detector = FeatureDetector.create(FeatureDetector.FAST);
		DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.ORB);
		DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);
		Mat frame = new Mat();
		capture.read(frame);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 1, 0);
		mainGrid.add(src2, 1, 1);

		MatOfKeyPoint[] oldKeypoints = new MatOfKeyPoint[] { new MatOfKeyPoint() };
		Mat[] oldDescriptors = new Mat[] { new Mat() };
		detector.detect(frame, oldKeypoints[0]);
		extractor.compute(frame, oldKeypoints[0], oldDescriptors[0]);

		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img deskewed = deskew(frame);
				src.setImage(Tools.mat2jfxImage(frame));
				deskewed.buildLayout().draw(deskewed, new Scalar(0, 255, 0), 1);
				src2.setImage(Tools.mat2jfxImage(deskewed.getSrc()));
			} catch (Exception e) {
				e.printStackTrace();

			}
		}, 0, 33, TimeUnit.MILLISECONDS);
	}

	private Img deskew(Mat frame) {
		try (Img img = new Img(frame, true);
				Img adaptativThreshold = img.cvtColor(Imgproc.COLOR_BGR2GRAY).adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 17, 9);
				Img closed = adaptativThreshold.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));) {
			double angle = detection_contours(frame, closed.getSrc());
			Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), angle, 1);
			Mat rotated = new Mat();
			Imgproc.warpAffine(frame, rotated, matrix, new Size(frame.size().width, frame.size().height));
			double crop = 0.15;
			Img result = new Img(new Mat(rotated, new Rect(Double.valueOf(rotated.width() * crop).intValue(), Double.valueOf(rotated.height() * crop).intValue(), Double.valueOf(rotated.width() * (1 - 2 * crop)).intValue(), Double.valueOf(
					rotated.height() * (1 - 2 * crop)).intValue())), false);
			matrix.release();
			rotated.release();
			return result;
		}
	}

	public double detection_contours(Mat frame, Mat dilated) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(dilated, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		double crop = 0.10;
		Predicate<RotatedRect> filter = rect -> rect.center.x > Double.valueOf(frame.width() * crop).intValue() && rect.center.y > Double.valueOf(frame.height() * crop).intValue() && rect.center.x < Double.valueOf(frame.width() * (1 - crop)).intValue()
				&& rect.center.y < Double.valueOf(frame.height() * (1 - crop)).intValue();
		List<RotatedRect> rotatedRects = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(contour -> Imgproc.minAreaRect(new MatOfPoint2f(contour.toArray()))).filter(filter).collect(Collectors.toList());
		double mean = 0;
		for (RotatedRect rotatedRect : rotatedRects) {
			if (rotatedRect.angle < -45.) {
				rotatedRect.angle += 90.0;
				double tmp = rotatedRect.size.width;
				rotatedRect.size.width = rotatedRect.size.height;
				rotatedRect.size.height = tmp;
			}
			mean += rotatedRect.angle;
		}
		final double average = mean / rotatedRects.size();

		List<RotatedRect> goodRects = rotatedRects.stream().filter(rotatedRect -> Math.abs(rotatedRect.angle - average) < 5).collect(Collectors.toList());
		double goodRectsMean = 0;
		for (RotatedRect rotatedRect : goodRects) {
			goodRectsMean += rotatedRect.angle;
		}
		final double goodAverage = goodRectsMean / goodRects.size();
		goodRects.forEach(rotatedRect -> rotatedRect.angle = goodAverage);
		System.out.println(average);
		System.out.println(goodAverage);

		goodRects.forEach(rotatedRect -> {
			Point[] result = new Point[4];
			rotatedRect.points(result);
			List<MatOfPoint> mof = Collections.singletonList(new MatOfPoint(new MatOfPoint(result)));
			Imgproc.drawContours(frame, mof, 0, new Scalar(0, 255, 0), 1);
			// Imgproc.drawContours(dilated, mof, 0, new Scalar(255), 1);
			});
		return goodAverage;
	}

	public void detection_deskew_contours(Mat frame, Mat dilated) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(dilated, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		List<Rect> rects = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(contour -> Imgproc.boundingRect(contour)).collect(Collectors.toList());
		rects.forEach(rect -> Imgproc.rectangle(frame, rect.tl(), rect.br(), new Scalar(0, 255, 0), 1));
	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}
}