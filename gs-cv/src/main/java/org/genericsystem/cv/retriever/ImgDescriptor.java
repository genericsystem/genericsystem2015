package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.lm.LevenbergImpl;
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
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.BFMatcher;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.FastFeatureDetector;
import org.opencv.imgproc.Imgproc;
import org.opencv.xfeatures2d.BriefDescriptorExtractor;

public class ImgDescriptor {
	private static final BriefDescriptorExtractor briefExtractor = BriefDescriptorExtractor.create(32, false);
	private static final FastFeatureDetector detector = FastFeatureDetector.create(10, true, FastFeatureDetector.TYPE_9_16);
	private static final DescriptorMatcher matcher = BFMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING, true);

	private final Img deperspectivedImg;
	private final MatOfKeyPoint keypoints = new MatOfKeyPoint();
	private final Mat descriptors;
	private final Mat homography;

	public ImgDescriptor(Mat frame, Mat deperspectivGraphy) {

		deperspectivedImg = CamLiveRetriever.warpPerspective(frame, deperspectivGraphy);
		detector.detect(deperspectivedImg.getSrc(), keypoints);

		// keypoints = detect(deperspectivedImg);
		assert keypoints != null && !keypoints.empty();
		descriptors = new Mat();
		briefExtractor.compute(deperspectivedImg.getSrc(), keypoints, descriptors);
		// EXTRACTOR.compute(deperspectivedImg.getSrc(), keypoints, descriptors);
		this.homography = deperspectivGraphy;

	}

	public Img getDeperspectivedImg() {
		return deperspectivedImg;
	}

	public MatOfKeyPoint getKeypoints() {
		return keypoints;
	}

	public Mat getDescriptors() {
		return descriptors;
	}

	public Mat getHomography() {
		return homography;
	}

	private static MatOfKeyPoint detect(Img frame) {
		Img closed = frame.bilateralFilter(5, 80, 80).adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
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

	public Mat computeStabilizationGraphy(ImgDescriptor frameDescriptor) {
		MatOfDMatch matches = new MatOfDMatch();
		// System.out.println(frameDescriptor.getDescriptors());
		matcher.match(getDescriptors(), frameDescriptor.getDescriptors(), matches);
		List<DMatch> goodMatches = new ArrayList<>();
		for (DMatch dMatch : matches.toArray()) {
			if (dMatch.distance <= 100) {
				goodMatches.add(dMatch);
			}
		}		

		List<KeyPoint> newKeypoints_ = frameDescriptor.getKeypoints().toList();
		List<KeyPoint> oldKeypoints_ = getKeypoints().toList();
		List<Point> goodNewKeypoints = new ArrayList<>();
		List<Point> goodOldKeypoints = new ArrayList<>();
		for (DMatch goodMatch : goodMatches) {
			goodNewKeypoints.add(newKeypoints_.get(goodMatch.trainIdx).pt);
			goodOldKeypoints.add(oldKeypoints_.get(goodMatch.queryIdx).pt);
		}

		if (goodMatches.size() > 30) {

			List<Point[]> pairedPoints = new ArrayList<>();
			for (int i = 0; i < goodNewKeypoints.size(); i++)
				pairedPoints.add(new Point[] { goodOldKeypoints.get(i), goodNewKeypoints.get(i) });

			double[] transScaleParams = new LevenbergImpl<>((points, params) -> distance(points, params), pairedPoints, new double[] { 1, 1, 0, 0 }).getParams();
			System.out.println("params " + Arrays.toString(transScaleParams));


			// Mat result = getTSMat(transScaleParams);

			Mat result = Calib3d.findHomography(new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), new MatOfPoint2f(goodNewKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 1);

			if (result.size().empty()) {
				CamLiveRetriever.logger.warn("Stabilization homography is empty");
				return null;
			}
			if (!isValidHomography(result)) {
				CamLiveRetriever.logger.warn("Not a valid homography");
				return null;
			}
			return result;
		} else {
			CamLiveRetriever.logger.warn("Not enough matches ({})", goodMatches.size());
			return null;
		}
	}

	private Mat getTSMat(double[] transScaleParams) {
		Mat TSMat = new Mat(3, 3, CvType.CV_64FC1, new Scalar(0));
		TSMat.put(0, 0, transScaleParams[0]);
		TSMat.put(1, 1, transScaleParams[1]);
		TSMat.put(0, 2, transScaleParams[2] * transScaleParams[0]);
		TSMat.put(1, 2, transScaleParams[3] * transScaleParams[1]);
		TSMat.put(2, 2, 1d);
		return TSMat;
	}

	private double distance(Point[] points, double[] params) {
		double p2x = points[1].x, p2y = points[1].y;
		double p1x = params[0] * points[0].x + params[0] * params[2];
		double p1y = params[1] * points[0].y + params[1] * params[3];
		double deltaX = p2x - p1x;
		double deltaY = p2y - p1y;
		double distance = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
		// System.out.println("distance: "+distance);
		return distance < 5 ? distance : 5;
	}

	/*
	 * When the homography is applied to a rectangle (clockwise decomposition), the transformed points must be in the same order (clockwise)
	 */
	private boolean isValidHomography(Mat homography) {
		int w = deperspectivedImg.getSrc().width();
		int h = deperspectivedImg.getSrc().height();
		MatOfPoint2f original = new MatOfPoint2f(new Point[] { new Point(0, 0), new Point(w, 0), new Point(w, h), new Point(0, h) });
		MatOfPoint2f dst = new MatOfPoint2f();
		Core.perspectiveTransform(original, dst, homography);
		List<Point> targets = dst.toList();
		return isClockwise(targets.get(0), targets.get(1), targets.get(2));
	}

	private boolean isClockwise(Point a, Point b, Point c) {
		double areaSum = 0;
		areaSum += a.x * (b.y - c.y);
		areaSum += b.x * (c.y - a.y);
		areaSum += c.x * (a.y - b.y);
		return areaSum > 0;
	}

}
