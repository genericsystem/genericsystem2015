package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GraphicApp.Label;
import org.genericsystem.cv.application.GraphicApp.Labels;
import org.genericsystem.cv.lm.LevenbergImpl;
import org.opencv.core.Core;
import org.opencv.core.CvType;
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

public class ImgDescriptor {
	private static final BriefDescriptorExtractor briefExtractor = BriefDescriptorExtractor.create(32, false);
	private static final FastFeatureDetector detector = FastFeatureDetector.create(10, true, FastFeatureDetector.TYPE_9_16);
	private static final DescriptorMatcher matcher = BFMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING, false);

	private final Img frame;
	private final MatOfKeyPoint keypoints = new MatOfKeyPoint();
	private final Mat descriptors;
	private final long timeStamp;
	private final Labels labels;

	public ImgDescriptor(Img frame, Labels labels) {
		this.frame = frame;
		this.labels = labels;
		detector.detect(frame.getSrc(), keypoints);
		assert keypoints != null && !keypoints.empty();
		descriptors = new Mat();
		briefExtractor.compute(frame.getSrc(), keypoints, descriptors);
		timeStamp = System.currentTimeMillis();
	}

	public Img getFrame() {
		return frame;
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

	public static class Link implements Comparable<Link> {
		private final Label label1;
		private final Label label2;

		public Link(Label label1, Label label2) {
			this.label1 = label1;
			this.label2 = label2;
		}

		@Override
		public int compareTo(Link link) {
			return Double.compare(link.distance(), distance());
		}

		private double distance() {
			return Math.sqrt(euclid(label1.getRect().tl(), label2.getRect().tl()) + euclid(label1.getRect().br(), label2.getRect().br()));
		}

		private double euclid(Point p1, Point p2) {
			return (p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y);
		}

		@Override
		public String toString() {
			return label1.getLabel() + " " + label2.getLabel();
		}

	}

	public Reconciliation computeReconciliation(ImgDescriptor reference) {

		Labels labels = new Labels(this.labels);
		Labels referenceLabels = new Labels(reference.getLabels());

		List<Link> links = new ArrayList<>();
		for (int i = 0; i < labels.getLabels().size(); i++)
			for (int j = 0; j < referenceLabels.getLabels().size(); j++)
				links.add(new Link(labels.getLabels().get(i), referenceLabels.getLabels().get(j)));
		links.removeIf(link -> !link.label1.getLabel().equals(link.label2.getLabel()));
		links.removeIf(link -> link.distance() > 10);

		System.out.println(links);

		MatOfDMatch matches = new MatOfDMatch();
		matcher.match(getDescriptors(), reference.getDescriptors(), matches);

		List<KeyPoint> referenceKeyPoints = reference.getKeypoints().toList();
		List<KeyPoint> keyPoints = getKeypoints().toList();
		List<Point> referencePts = new ArrayList<>();
		List<Point> pts = new ArrayList<>();
		for (DMatch goodMatch : matches.toArray())
			if (goodMatch.distance <= 150) {
				referencePts.add(referenceKeyPoints.get(goodMatch.trainIdx).pt);
				pts.add(keyPoints.get(goodMatch.queryIdx).pt);
			}
		if (referencePts.size() > 50) {

			List<Point[]> pairedPoints = new ArrayList<>();
			for (int i = 0; i < pts.size(); i++)
				pairedPoints.add(new Point[] { pts.get(i), referencePts.get(i) });

			double[] transScaleParams = new LevenbergImpl<>((points, params) -> distance(points, params), pairedPoints, new double[] { 1, 1, 0, 0 }).getParams();
			System.out.println("params " + Arrays.toString(transScaleParams));
			Mat result = Mat.zeros(3, 3, CvType.CV_64FC1);
			result.put(0, 0, transScaleParams[0]);
			result.put(1, 1, transScaleParams[1]);
			result.put(0, 2, transScaleParams[2] * transScaleParams[0]);
			result.put(1, 2, transScaleParams[3] * transScaleParams[1]);
			result.put(2, 2, 1);
			// Mat result = Calib3d.findHomography(new MatOfPoint2f(pts.stream().toArray(Point[]::new)), new MatOfPoint2f(referencePts.stream().toArray(Point[]::new)), Calib3d.RANSAC, 1);
			if (result.size().empty()) {
				System.out.println("Stabilization homography is empty");
				return null;
			}
			double error = 0;
			for (Point[] points : pairedPoints) {
				error += Math.pow(distance(points, transScaleParams), 2);
			}
			error = Math.sqrt(error) / pairedPoints.size();
			if (error > 5) {
				System.out.println("error too big : " + error + " match size : " + pairedPoints.size());
				return null;
			}
			System.out.println("error : " + error + " match size : " + pairedPoints.size());
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

	private Labels getLabels() {
		return labels;
	}

	private double distance(Point[] points, double[] params) {
		double sx = params[0];
		double sy = params[1];
		double tx = params[2];
		double ty = params[3];

		double dx = sx * (points[0].x + tx) - points[1].x;
		double dy = sy * (points[0].y + ty) - points[1].y;

		return Math.sqrt(dx * dx + dy * dy);
	}

	private boolean isValidHomography(Mat homography) {
		int w = frame.width();
		int h = frame.height();
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
