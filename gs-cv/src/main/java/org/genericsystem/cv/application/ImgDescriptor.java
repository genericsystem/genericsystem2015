package org.genericsystem.cv.application;

import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.DMatch;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDMatch;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.features2d.BFMatcher;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.FastFeatureDetector;
import org.opencv.xfeatures2d.BriefDescriptorExtractor;

import java.util.ArrayList;
import java.util.List;

public class ImgDescriptor {
	private static final BriefDescriptorExtractor briefExtractor = BriefDescriptorExtractor.create(32, false);
	private static final FastFeatureDetector detector = FastFeatureDetector.create(10, true, FastFeatureDetector.TYPE_9_16);
	private static final DescriptorMatcher matcher = BFMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING, false);

	private final SuperFrameImg superFrame;
	private final MatOfKeyPoint keypoints = new MatOfKeyPoint();
	private final Mat descriptors;
	private final long timeStamp;

	public ImgDescriptor(SuperFrameImg superFrame) {
		this.superFrame = superFrame;
		detector.detect(superFrame.getFrame().getSrc(), keypoints);
		// keypoints = detect(deperspectivedImg);
		assert keypoints != null && !keypoints.empty();
		descriptors = new Mat();
		briefExtractor.compute(superFrame.getFrame().getSrc(), keypoints, descriptors);
		// EXTRACTOR.compute(deperspectivedImg.getSrc(), keypoints, descriptors);
		timeStamp = System.currentTimeMillis();
	}

	public SuperFrameImg getSuperFrame() {
		return superFrame;
	}

	public Mat getDescriptors() {
		return descriptors;
	}

	public MatOfKeyPoint getKeypoints() {
		return keypoints;
	}

	public long getTimeStamp() {
		return timeStamp;
	}

	public Reconciliation computeReconciliation(ImgDescriptor reference) {
		MatOfDMatch matches = new MatOfDMatch();
		matcher.match(getDescriptors(), reference.getDescriptors(), matches);

		List<KeyPoint> referenceKeyPoints = reference.getKeypoints().toList();
		List<KeyPoint> keyPoints = getKeypoints().toList();
		List<Point> referencePts = new ArrayList<>();
		List<Point> pts = new ArrayList<>();
		for (DMatch goodMatch : matches.toArray())
			if (goodMatch.distance <= 120) {
				referencePts.add(referenceKeyPoints.get(goodMatch.trainIdx).pt);
				pts.add(keyPoints.get(goodMatch.queryIdx).pt);
			}
		if (referencePts.size() > 50) {

			// List<Point[]> pairedPoints = new ArrayList<>();
			// for (int i = 0; i < goodNewKeypoints.size(); i++)
			// pairedPoints.add(new Point[] { goodOldKeypoints.get(i), goodNewKeypoints.get(i) });

			// double[] transScaleParams = new LevenbergImpl<>((points, params) -> distance(points, params), pairedPoints, new double[] { 1, 1, 0, 0 }).getParams();
			// System.out.println("params " + Arrays.toString(transScaleParams));
			// Mat result = getTSMat(transScaleParams);

			Mat result = Calib3d.findHomography(new MatOfPoint2f(pts.stream().toArray(Point[]::new)), new MatOfPoint2f(referencePts.stream().toArray(Point[]::new)), Calib3d.RANSAC, 1);
			if (result.size().empty()) {
				System.out.println("Stabilization homography is empty");
				return null;
			}
			if (!isValidHomography(result)) {
				System.out.println("Not a valid homography");
				return null;
			}
			return new Reconciliation(result, pts, referencePts);
		} else {
			// System.out.println("Not enough matches (" + referencePts.size() + ")");
			return null;
		}
	}

	private boolean isValidHomography(Mat homography) {
		int w = superFrame.getFrame().width();
		int h = superFrame.getFrame().height();
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
