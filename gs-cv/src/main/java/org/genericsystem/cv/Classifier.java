package org.genericsystem.cv;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.DMatch;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDMatch;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Size;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.FeatureDetector;
import org.opencv.features2d.Features2d;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

public class Classifier {

	private final static String alignedDirectoryPath = "aligned";
	private final static int MATCHING_THRESHOLD = 150;

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static Mat compareFeature(String filename1, String filename2) {
		Mat img1 = Imgcodecs.imread(filename1, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		Mat img2 = Imgcodecs.imread(filename2, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		Mat result = compareFeature(img1, img2, MATCHING_THRESHOLD);
		if (result != null) {
			String dir = alignedDirectoryPath + "-" + filename2.replaceFirst(".*/", "");
			new File(dir).mkdirs();
			Imgcodecs.imwrite(dir + "/" + filename1.replaceFirst(".*/", ""), result);
		}
		return result;
	}

	public static Mat compareFeature(Mat img1, Mat img2, int matching_threshold) {
		long startTime = System.currentTimeMillis();

		// Declare key point of images
		MatOfKeyPoint keypoints1 = new MatOfKeyPoint();
		MatOfKeyPoint keypoints2 = new MatOfKeyPoint();
		Mat descriptors1 = new Mat();
		Mat descriptors2 = new Mat();

		// Definition of ORB key point detector and descriptor extractors
		FeatureDetector detector = FeatureDetector.create(FeatureDetector.BRISK);
		DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.OPPONENT_ORB);

		// Detect key points
		detector.detect(img1, keypoints1);
		detector.detect(img2, keypoints2);

		// Extract descriptors
		extractor.compute(img1, keypoints1, descriptors1);
		extractor.compute(img2, keypoints2, descriptors2);

		System.out.println("Type of Image1= " + descriptors1.type() + ", Type of Image2= " + descriptors2.type());
		System.out.println("Cols of Image1= " + descriptors1.cols() + ", Cols of Image2= " + descriptors2.cols());

		Mat transformedImage = null;
		if (descriptors2.cols() == descriptors1.cols()) {
			// Definition of descriptor matcher
			DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);

			// Match points of two images
			MatOfDMatch matches = new MatOfDMatch();
			matcher.match(descriptors1, descriptors2, matches);

			// Check matches of key points
			DMatch[] match = matches.toArray();
			double max_dist = 0;
			double min_dist = 100;

			for (int i = 0; i < descriptors1.rows(); i++) {
				double dist = match[i].distance;
				if (dist < min_dist)
					min_dist = dist;
				if (dist > max_dist)
					max_dist = dist;
			}
			System.out.println("max_dist=" + max_dist + ", min_dist=" + min_dist);

			// Extract good images (distances are under 10)
			List<DMatch> goodMatches = new ArrayList<>();
			for (int i = 0; i < descriptors1.rows(); i++) {
				if (match[i].distance <= 30) {
					goodMatches.add(match[i]);
				}
			}
			if (goodMatches.size() > matching_threshold) {
				Mat imgMatches = new Mat();
				Features2d.drawMatches(img1, keypoints1, img2, keypoints2, new MatOfDMatch(goodMatches.stream().toArray(DMatch[]::new)), imgMatches);
				List<Point> objectPoints = new ArrayList<>();
				List<Point> scenePoints = new ArrayList<>();
				for (DMatch goodMatch : goodMatches) {
					objectPoints.add(keypoints1.toList().get(goodMatch.queryIdx).pt);
					scenePoints.add(keypoints2.toList().get(goodMatch.trainIdx).pt);
				}

				Mat homography = Calib3d.findHomography(new MatOfPoint2f(objectPoints.stream().toArray(Point[]::new)), new MatOfPoint2f(scenePoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
				transformedImage = new Mat();
				Imgproc.warpPerspective(img1, transformedImage, homography, new Size(img2.cols(), img2.rows()));
			}
			System.out.println("matching count=" + goodMatches.size());
		}

		long estimatedTime = System.currentTimeMillis() - startTime;
		System.out.println("estimatedTime=" + estimatedTime + "ms");

		return transformedImage;
	}

}
