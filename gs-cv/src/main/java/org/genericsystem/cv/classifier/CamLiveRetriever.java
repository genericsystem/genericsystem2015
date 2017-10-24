package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.Deskewer;
import org.genericsystem.cv.utils.Deskewer.METHOD;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
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

public class CamLiveRetriever extends AbstractApp {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private MatOfKeyPoint oldKeypoints;
	private MatOfKeyPoint newKeypoints;
	private Mat oldDescriptors;
	private Mat newDescriptors;
	private final Fields fields = new Fields();
	private boolean stabilizationHasChanged = true;

	Mat homography = null;
	Mat frame = new Mat();
	double angle = 0;

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private Img stabilized = null;
	private final ScheduledExecutorService timerFields = Executors.newSingleThreadScheduledExecutor();
	private final ScheduledExecutorService timerOcr = Executors.newSingleThreadScheduledExecutor();

	@Override
	public void stop() throws Exception {
		super.stop();
		timerFields.shutdown();
		timerFields.awaitTermination(5, TimeUnit.SECONDS);
		timerOcr.shutdown();
		timerOcr.awaitTermination(5, TimeUnit.SECONDS);
		capture.release();
		stabilized.close();
		oldKeypoints.release();
		newKeypoints.release();
		oldDescriptors.release();
		newDescriptors.release();
		homography.release();
		frame.release();
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		// FeatureDetector detector = FeatureDetector.create(FeatureDetector.FAST);
		DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.ORB);
		DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);
		capture.read(frame);

		ImageView src0 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src0, 0, 0);

		ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src1, 0, 1);

		oldKeypoints = new MatOfKeyPoint();
		oldDescriptors = new Mat();

		// Stabilize the image
		timerFields.scheduleAtFixedRate(() -> onSpace(), 0, 110, TimeUnit.MILLISECONDS);

		// Detect the rectangles
		timerFields.scheduleAtFixedRate(() -> {
			synchronized (this) {
				try {
					stabilized = getStabilized(frame, extractor, matcher);
					if (stabilized != null) {
						if (stabilizationHasChanged) {
							List<Rect> newRects = detectRects(stabilized);
							fields.merge(newRects);
							stabilizationHasChanged = false;
						}
						Img display = new Img(frame, true);
						Img stabilizedDisplay = new Img(stabilized.getSrc(), true);

						fields.drawOcrPerspectiveInverse(display, homography.inv(), new Scalar(0, 64, 255), 1);
						fields.drawConsolidated(stabilizedDisplay);
						src0.setImage(display.toJfxImage());
						src1.setImage(stabilizedDisplay.toJfxImage());

						display.close();
						stabilizedDisplay.close();
					}
				} catch (Throwable e) {
					logger.warn("Exception while computing layout.", e);
				}
			}
		}, 500, 33, TimeUnit.MILLISECONDS);

		// Perform the OCR
		timerOcr.scheduleAtFixedRate(() -> consolidateOcr(), 1000, 200, TimeUnit.MILLISECONDS);

	}

	private synchronized void consolidateOcr() {
		try {
			if (stabilized != null) {
				fields.consolidateOcr(stabilized);
			}
		} catch (Throwable e) {
			logger.warn("Exception while computing OCR", e);
		}
	}

	@Override
	protected synchronized void onSpace() {
		if (homography != null) {
			fields.storeLastHomography(homography.inv());
			fields.storeLastRotation(Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), angle, 1));
		}
		oldKeypoints = newKeypoints;
		oldDescriptors = newDescriptors;
		stabilizationHasChanged = true;
	}

	private Img getStabilized(Mat frame, DescriptorExtractor extractor, DescriptorMatcher matcher) {
		Mat stabilizedMat = new Mat();
		capture.read(frame);
		Img frameImg = new Img(frame, false);
		frameImg = frameImg.bilateralFilter(5, 100, 100);
		Img deskewed_ = deskew(frameImg);
		newKeypoints = detect(deskewed_);
		newDescriptors = new Mat();
		extractor.compute(deskewed_.getSrc(), newKeypoints, newDescriptors);
		Img stabilized = stabilize(stabilizedMat, matcher);
		frameImg.close();
		deskewed_.close();
		return stabilized;
	}

	private Img stabilize(Mat stabilized, DescriptorMatcher matcher) {
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
				homography = Calib3d.findHomography(originalNewPoints, new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
				Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
				Mat maskWarpped = new Mat();
				Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
				Mat tmp = new Mat();
				Imgproc.warpPerspective(frame, tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
				tmp.copyTo(stabilized, maskWarpped);
				return new Img(stabilized, false);
			}
		}
		System.out.println("No stabilized image");
		return null;

	}

	private List<Rect> detectRects(Img stabilized) {
		List<MatOfPoint> contours = new ArrayList<>();
		Img closed = stabilized.adaptativeGaussianInvThreshold(7, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(9, 1));
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		List<Rect> res = contours.stream().filter(contour -> Imgproc.contourArea(contour) > 200).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList());
		return res;
	}

	private MatOfKeyPoint detect(Img frame) {
		Img closed = frame.adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		List<KeyPoint> keyPoints = new ArrayList<>();
		contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(Imgproc::boundingRect).forEach(rect -> {
			keyPoints.add(new KeyPoint((float) rect.tl().x, (float) rect.tl().y, 6));
			keyPoints.add(new KeyPoint((float) rect.tl().x, (float) rect.br().y, 6));
			keyPoints.add(new KeyPoint((float) rect.br().x, (float) rect.tl().y, 6));
			keyPoints.add(new KeyPoint((float) rect.br().x, (float) rect.br().y, 6));
		});
		return new MatOfKeyPoint(keyPoints.stream().toArray(KeyPoint[]::new));
	}

	private Img deskew(Img frame) {
		Img closed = frame.adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		angle = Deskewer.detectAngle(closed.getSrc(), METHOD.HOUGH_LINES);
		Mat matrix = Imgproc.getRotationMatrix2D(new Point(frame.width() / 2, frame.height() / 2), angle, 1);
		Mat rotated = new Mat(frame.size(), CvType.CV_8UC3, new Scalar(255, 255, 255));
		Imgproc.warpAffine(frame.getSrc(), rotated, matrix, frame.size());
		closed.close();
		matrix.release();
		return new Img(rotated);
	}

}