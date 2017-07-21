package org.genericsystem.layout;

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

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Tools;
import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.DMatch;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDMatch;
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
		// FeatureDetector detector = FeatureDetector.create(FeatureDetector.FAST);
		DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.ORB);
		DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);
		Mat frame = new Mat();
		capture.read(frame);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		Img deskewed = deskew(frame);

		ImageView src2 = new ImageView(deskewed.toJfxImage());
		ImageView src3 = new ImageView(deskewed.toJfxImage());
		mainGrid.add(src, 0, 0);
		mainGrid.add(src2, 0, 1);
		mainGrid.add(src3, 0, 2);
		MatOfKeyPoint[] oldKeypoints = new MatOfKeyPoint[] { detect(deskewed.getSrc()) };
		Mat[] oldDescriptors = new Mat[] { new Mat() };
		extractor.compute(deskewed.bgr2Gray().getSrc(), oldKeypoints[0], oldDescriptors[0]);
		int[] count = new int[] { 0 };
		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img deskewed_ = deskew(frame);
				src.setImage(deskewed_.toJfxImage());
				MatOfKeyPoint newKeypoints = detect(deskewed_.getSrc());
				Mat newDescriptors = new Mat();
				extractor.compute(deskewed_.bgr2Gray().getSrc(), newKeypoints, newDescriptors);
				MatOfDMatch matches = new MatOfDMatch();
				matcher.match(oldDescriptors[0], newDescriptors, matches);
				List<DMatch> goodMatches = new ArrayList<>();
				DMatch[] matches_ = matches.toArray();
				for (DMatch dMatch : matches_) {
					if (dMatch.distance <= 40) {
						goodMatches.add(dMatch);
					}
				}
				List<KeyPoint> newKeypoints_ = newKeypoints.toList();
				List<KeyPoint> oldKeypoints_ = oldKeypoints[0].toList();
				System.out.println(goodMatches.size() + " " + newKeypoints_.size() + " " + oldKeypoints_.size());

				List<Point> goodNewKeypoints = new ArrayList<>();
				List<Point> goodOldKeypoints = new ArrayList<>();
				for (DMatch goodMatch : goodMatches) {
					goodNewKeypoints.add(newKeypoints_.get(goodMatch.trainIdx).pt);
					goodOldKeypoints.add(oldKeypoints_.get(goodMatch.queryIdx).pt);
				}
				// System.out.println("------------------------------------------");
				// System.out.println(goodNewKeypoints);
				// System.out.println(goodOldKeypoints);

				if (goodMatches.size() > 30) {
					Mat homography = Calib3d.findHomography(new MatOfPoint2f(goodNewKeypoints.stream().toArray(Point[]::new)), new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
					Mat transformedImage = new Mat();
					Imgproc.warpPerspective(deskewed_.getSrc(), transformedImage, homography, new Size(deskewed_.getSrc().cols(), deskewed_.getSrc().rows()));
					Img stabilized = new Img(transformedImage);
					stabilized.buildLayout().draw(stabilized, new Scalar(0, 255, 0), 1);
					src2.setImage(stabilized.toJfxImage());
				}

				deskewed_.buildLayout().draw(deskewed_, new Scalar(0, 255, 0), 1);
				src3.setImage(Tools.mat2jfxImage(deskewed_.getSrc()));
				count[0]++;
				if ((count[0] % 100) == 0) {
					oldKeypoints[0] = newKeypoints;
					oldDescriptors[0] = newDescriptors;
				}
			} catch (Exception e) {
				e.printStackTrace();

			}
		}, 0, 33, TimeUnit.MILLISECONDS);
	}

	private MatOfKeyPoint detect(Mat src) {
		try (Img img = new Img(src);
				Img adaptativThreshold = img.cvtColor(Imgproc.COLOR_BGR2GRAY).adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 17, 9);
				Img closed = adaptativThreshold.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));) {
			List<KeyPoint> keyPoints = new ArrayList<>();
			List<MatOfPoint> contours = new ArrayList<>();
			Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
			double minArea = 100;
			List<Rect> rects = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(contour -> Imgproc.boundingRect(contour)).collect(Collectors.toList());
			rects.forEach(rect -> {
				keyPoints.add(new KeyPoint(Double.valueOf(rect.tl().x).floatValue(), Double.valueOf(rect.tl().y).floatValue(), 6));
				keyPoints.add(new KeyPoint(Double.valueOf(rect.tl().x).floatValue(), Double.valueOf(rect.br().y).floatValue(), 6));
				keyPoints.add(new KeyPoint(Double.valueOf(rect.br().x).floatValue(), Double.valueOf(rect.tl().y).floatValue(), 6));
				keyPoints.add(new KeyPoint(Double.valueOf(rect.br().x).floatValue(), Double.valueOf(rect.br().y).floatValue(), 6));
			});
			return new MatOfKeyPoint(keyPoints.stream().toArray(KeyPoint[]::new));
		}
	}

	private Img deskew(Mat frame) {
		try (Img img = new Img(frame);
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

	private double detection_contours(Mat frame, Mat dilated) {
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
		// System.out.println(average);
		// System.out.println(goodAverage);

		goodRects.forEach(rotatedRect -> {
			Point[] result = new Point[4];
			rotatedRect.points(result);
			List<MatOfPoint> mof = Collections.singletonList(new MatOfPoint(new MatOfPoint(result)));
			// Imgproc.drawContours(frame, mof, 0, new Scalar(0, 255, 0), 1);
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