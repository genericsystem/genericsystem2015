package org.genericsystem.layout;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Tools;
import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.CvType;
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
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class CamLayoutAnalyzer extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private double crop = 0.15;

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
		ImageView src0 = new ImageView(Tools.mat2jfxImage(frame));
		ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		Img deskewed = deskew(frame, new double[1]);
		ImageView src2 = new ImageView(deskewed.toJfxImage());
		ImageView src3 = new ImageView(deskewed.toJfxImage());
		mainGrid.add(src0, 0, 0);
		mainGrid.add(src1, 0, 1);
		mainGrid.add(src2, 1, 0);
		mainGrid.add(src3, 1, 1);

		MatOfKeyPoint[] oldKeypoints = new MatOfKeyPoint[] { detect(deskewed.getSrc()) };
		Mat[] oldDescriptors = new Mat[] { new Mat() };
		extractor.compute(deskewed.getSrc(), oldKeypoints[0], oldDescriptors[0]);
		int[] count = new int[] { 0 };
		Mat stabilizedMat = new Mat();
		Layout[] layout = new Layout[] { null };
		timer.scheduleAtFixedRate(() -> {
			MatOfKeyPoint newKeypoints = null;
			Mat newDescriptors = null;
			try {
				capture.read(frame);
				double[] angle = new double[1];
				Size newSize = new Size(frame.width() * (1 - 2 * crop), frame.height() * (1 - 2 * crop));
				Img frameImg = new Img(new Mat(frame, new Rect(Double.valueOf(crop * frame.width()).intValue(), Double.valueOf(crop * frame.height()).intValue(), Double.valueOf(newSize.width).intValue(), Double.valueOf(newSize.height).intValue())), true);
				src0.setImage(frameImg.toJfxImage());
				Img deskewed_ = deskew(frame, angle);

				Img deskiewedImg = new Img(
						new Mat(deskewed_.getSrc(), new Rect(Double.valueOf(crop * frame.width()).intValue(), Double.valueOf(crop * frame.height()).intValue(), Double.valueOf(newSize.width).intValue(), Double.valueOf(newSize.height).intValue())), true);

				src1.setImage(deskiewedImg.toJfxImage());
				newKeypoints = detect(deskewed_.getSrc());
				newDescriptors = new Mat();
				extractor.compute(deskewed_.getSrc(), newKeypoints, newDescriptors);

				Img deskiewedCopy = new Img(deskiewedImg.getSrc(), true);
				deskiewedImg.buildLayout().draw(deskiewedCopy, new Scalar(0, 255, 0), 1);
				src2.setImage(deskiewedCopy.toJfxImage());

				Img stabilized = stabilize(frame, stabilizedMat, newSize, matcher, oldKeypoints[0], newKeypoints, oldDescriptors[0], newDescriptors, angle[0], crop);
				if (stabilized != null) {
					Img stabilizedCopy = new Img(stabilized.getSrc(), true);
					if (layout[0] == null)
						layout[0] = stabilized.buildLayout();
					layout[0].draw(stabilizedCopy, new Scalar(0, 255, 0), 1);
					src3.setImage(stabilizedCopy.toJfxImage());
				}

				count[0]++;
			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				if ((count[0] % 10) == 0) {
					oldKeypoints[0] = newKeypoints;
					oldDescriptors[0] = newDescriptors;
					layout[0] = null;
				}
			}
		}, 0, 66, TimeUnit.MILLISECONDS);
	}

	private Img stabilize(Mat frame, Mat stabilized, Size size, DescriptorMatcher matcher, MatOfKeyPoint oldKeypoints, MatOfKeyPoint newKeypoints, Mat oldDescriptors, Mat newDescriptors, double angle, double crop) {
		MatOfDMatch matches = new MatOfDMatch();
		matcher.match(oldDescriptors, newDescriptors, matches);
		List<DMatch> goodMatches = new ArrayList<>();
		for (DMatch dMatch : matches.toArray()) {
			if (dMatch.distance <= 40) {
				goodMatches.add(dMatch);
			}
		}
		List<KeyPoint> newKeypoints_ = newKeypoints.toList();
		List<KeyPoint> oldKeypoints_ = oldKeypoints.toList();
		// System.out.println(goodMatches.size() + " " + newKeypoints_.size() + " " + oldKeypoints_.size());

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
			Mat goodNewPoints = Converters.vector_Point2f_to_Mat(goodNewKeypoints);
			MatOfPoint2f originalNewPoints = new MatOfPoint2f();
			Core.transform(goodNewPoints, originalNewPoints, Imgproc.getRotationMatrix2D(new Point(size.width / 2, size.height / 2), -angle, 1));
			List<Point> shiftPoints = new ArrayList<>();
			Converters.Mat_to_vector_Point2f(originalNewPoints, shiftPoints);
			for (int i = 0; i < shiftPoints.size(); i++) {
				double x = shiftPoints.get(i).x + crop * size.width / (1 - 2 * crop);
				double y = shiftPoints.get(i).y + crop * size.height / (1 - 2 * crop);
				shiftPoints.set(i, new Point(x, y));
			}
			Mat homography = Calib3d.findHomography(new MatOfPoint2f(shiftPoints.stream().toArray(Point[]::new)), new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
			Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
			Mat maskWarpped = new Mat();
			Imgproc.warpPerspective(mask, maskWarpped, homography, size);
			Mat tmp = new Mat();
			Imgproc.warpPerspective(frame, tmp, homography, size, Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
			tmp.copyTo(stabilized, maskWarpped);
			return new Img(stabilized, false);
		}
		return null;

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

	private Img deskew(Mat frame, double[] angle) {
		try (Img img = new Img(frame);
				Img adaptativThreshold = img.cvtColor(Imgproc.COLOR_BGR2GRAY).adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 17, 9);
				Img closed = adaptativThreshold.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));) {
			angle[0] = detection_contours(frame, closed.getSrc());
			Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), angle[0], 1);
			Mat rotated = new Mat();
			Imgproc.warpAffine(frame, rotated, matrix, frame.size());
			// matrix.release();
			// rotated.release();
			return new Img(rotated);
		}
	}

	private double detection_contours(Mat frame, Mat dilated) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(dilated, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
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

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}
}