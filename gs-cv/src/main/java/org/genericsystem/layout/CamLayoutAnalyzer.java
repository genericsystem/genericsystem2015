package org.genericsystem.layout;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Tools;
import org.genericsystem.cv.utils.NativeLibraryLoader;
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
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class CamLayoutAnalyzer extends AbstractApp {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private MatOfKeyPoint oldKeypoints;
	private MatOfKeyPoint newKeypoints;
	private Mat oldDescriptors;
	private Mat newDescriptors;
	private Layout layout;

	static {
		NativeLibraryLoader.load();
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
		ImageView src0 = new ImageView(Tools.mat2jfxImage(frame));
		// ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		// ImageView src2 = new ImageView(Tools.mat2jfxImage(frame));
		ImageView src3 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src0, 0, 0);
		// mainGrid.add(src1, 0, 1);
		// mainGrid.add(src2, 1, 0);
		mainGrid.add(src3, 0, 1);

		oldKeypoints = new MatOfKeyPoint();
		oldDescriptors = new Mat();
		Mat stabilizedMat = new Mat();
		timer.scheduleAtFixedRate(() -> {
			Img frameImg = new Img(frame, false);
			synchronized (this) {
				try {
					Mat[] homography = new Mat[1];
					double[] angle = new double[1];
					capture.read(frame);
					Img deskewed_ = deskew(frameImg, angle);
					newKeypoints = detect(deskewed_);
					newDescriptors = new Mat();
					extractor.compute(deskewed_.getSrc(), newKeypoints, newDescriptors);
					// Img deskiewedCopy = new Img(deskewed_.getSrc(), true);
					Img binary = deskewed_/* .cleanFaces(0.1, 0.26) */.adaptativeGaussianThreshold(17, 7).cleanTables(0.05);
					// binary.buildLayout().draw(deskiewedCopy, new Scalar(0, 255, 0), 1);
					Img stabilized = stabilize(frame, stabilizedMat, matcher, angle[0], homography);
					if (stabilized != null) {
						Img stabilizedCopy = new Img(stabilized.getSrc(), true);
						if (layout == null) {
							Img binary2 = stabilized/* .cleanFaces(0.1, 0.26) */.adaptativeGaussianThreshold(17, 7).cleanTables(0.05);
							layout = binary2.buildLayout();
						}
						layout.ocrTree(stabilizedCopy, 0.03, 0.1);
						layout.draw(stabilizedCopy, new Scalar(0, 255, 0), 1);
						layout.drawPerspective(frameImg, homography[0].inv(), new Scalar(0, 0, 255), 1, 3, 3);
						double area = layout.area(stabilized);
						Imgproc.putText(stabilizedCopy.getSrc(), "Surface : " + area, new Point(0.5 * stabilizedCopy.width(), 0.05 * stabilizedCopy.height()), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 0, 0), 1);
						src0.setImage(frameImg.toJfxImage());
						// src1.setImage(deskewed_.toJfxImage());
						// src2.setImage(deskiewedCopy.toJfxImage());
						src3.setImage(stabilizedCopy.toJfxImage());
					}
				} catch (Throwable e) {
					logger.warn("Exception while computing layout.", e);
				}
			}
		}, 500, 66, TimeUnit.MILLISECONDS);

		timer.scheduleAtFixedRate(() -> onSpace(), 1000, 1000, TimeUnit.MILLISECONDS);
		onSpace();
	}

	@Override
	protected synchronized void onSpace() {
		oldKeypoints = newKeypoints;
		oldDescriptors = newDescriptors;
		layout = null;
	}

	private Img stabilize(Mat frame, Mat stabilized, DescriptorMatcher matcher, double angle, Mat[] homography) {
		MatOfDMatch matches = new MatOfDMatch();
		if (oldDescriptors != null && !oldDescriptors.empty() && (!newDescriptors.empty())) {
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

			if (goodMatches.size() > 30) {
				Mat goodNewPoints = Converters.vector_Point2f_to_Mat(goodNewKeypoints);
				MatOfPoint2f originalNewPoints = new MatOfPoint2f();
				Core.transform(goodNewPoints, originalNewPoints, Imgproc.getRotationMatrix2D(new Point(frame.size().width / 2, frame.size().height / 2), -angle, 1));
				homography[0] = Calib3d.findHomography(originalNewPoints, new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
				Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
				Mat maskWarpped = new Mat();
				Imgproc.warpPerspective(mask, maskWarpped, homography[0], frame.size());
				Mat tmp = new Mat();
				Imgproc.warpPerspective(frame, tmp, homography[0], frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
				tmp.copyTo(stabilized, maskWarpped);
				return new Img(stabilized, false);
			}
		}
		return null;

	}

	private MatOfKeyPoint detect(Img frame) {
		Img closed = frame.adaptativeGaussianInvThreshold(17, 9).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		List<KeyPoint> keyPoints = new ArrayList<>();
		contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(Imgproc::boundingRect).collect(Collectors.toList()).forEach(rect -> {
			keyPoints.add(new KeyPoint(Double.valueOf(rect.tl().x).floatValue(), Double.valueOf(rect.tl().y).floatValue(), 6));
			keyPoints.add(new KeyPoint(Double.valueOf(rect.tl().x).floatValue(), Double.valueOf(rect.br().y).floatValue(), 6));
			keyPoints.add(new KeyPoint(Double.valueOf(rect.br().x).floatValue(), Double.valueOf(rect.tl().y).floatValue(), 6));
			keyPoints.add(new KeyPoint(Double.valueOf(rect.br().x).floatValue(), Double.valueOf(rect.br().y).floatValue(), 6));
		});
		return new MatOfKeyPoint(keyPoints.stream().toArray(KeyPoint[]::new));
	}

	private Img deskew(Img frame, double[] angle) {
		Img closed = frame.adaptativeGaussianInvThreshold(17, 9).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		angle[0] = detection_contours(frame.getSrc(), closed.getSrc());
		Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), angle[0], 1);
		Mat rotated = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		Imgproc.warpAffine(frame.getSrc(), rotated, matrix, frame.size());
		return new Img(rotated);
	}

	private double detection_contours(Mat frame, Mat dilated) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(dilated, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		double crop = 0;
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
		for (RotatedRect rotatedRect : goodRects)
			goodRectsMean += rotatedRect.angle;
		final double goodAverage = goodRectsMean / goodRects.size();
		// goodRects.forEach(rotatedRect -> rotatedRect.angle = goodAverage);
		// goodRects.forEach(rotatedRect -> {
		// Point[] result = new Point[4];
		// rotatedRect.points(result);
		// List<MatOfPoint> mof = Collections.singletonList(new MatOfPoint(new MatOfPoint(result)));
		// // Imgproc.drawContours(frame, mof, 0, new Scalar(0, 255, 0), 1);
		// // Imgproc.drawContours(dilated, mof, 0, new Scalar(255), 1);
		// });
		// System.out.println(goodAverage - average);
		return goodAverage;
	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}
}