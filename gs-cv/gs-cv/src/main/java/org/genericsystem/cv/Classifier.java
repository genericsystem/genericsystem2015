package org.genericsystem.cv;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.DMatch;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDMatch;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.FeatureDetector;
import org.opencv.imgcodecs.Imgcodecs;

public class Classifier {

	private final static String sourceDirectoryPath = "png";

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		doWork(sourceDirectoryPath);
	}

	public static void doWork(String sourceDirectoryPath) {
		List<File> images = Arrays.stream(new File(sourceDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).collect(Collectors.toList());
		for (File img1 : images) {
			for (File img2 : images) {
				compareFeature(img1.getPath(), img2.getPath());
			}
		}

	};

	public static int compareFeature(String filename1, String filename2) {
		int retVal = 0;
		long startTime = System.currentTimeMillis();

		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);

		// Load images to compare
		Mat img1 = Imgcodecs.imread(filename1, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		Mat img2 = Imgcodecs.imread(filename2, Imgcodecs.CV_LOAD_IMAGE_COLOR);

		// Declare key point of images
		MatOfKeyPoint keypoints1 = new MatOfKeyPoint();
		MatOfKeyPoint keypoints2 = new MatOfKeyPoint();
		Mat descriptors1 = new Mat();
		Mat descriptors2 = new Mat();

		// Definition of ORB key point detector and descriptor extractors
		FeatureDetector detector = FeatureDetector.create(FeatureDetector.ORB);
		DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.ORB);

		// Detect key points
		detector.detect(img1, keypoints1);
		detector.detect(img2, keypoints2);

		// Extract descriptors
		extractor.compute(img1, keypoints1, descriptors1);
		extractor.compute(img2, keypoints2, descriptors2);

		// Definition of descriptor matcher
		DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);

		// Match points of two images
		MatOfDMatch matches = new MatOfDMatch();
		System.out.println("Type of Image1= " + descriptors1.type() + ", Type of Image2= " + descriptors2.type());
		System.out.println("Cols of Image1= " + descriptors1.cols() + ", Cols of Image2= " + descriptors2.cols());

		// Avoid to assertion failed
		// Assertion failed (type == src2.type() && src1.cols == src2.cols && (type == CV_32F || type == CV_8U)

		if (descriptors2.cols() == descriptors1.cols()) {
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
			for (int i = 0; i < descriptors1.rows(); i++) {
				if (match[i].distance <= 10) {
					retVal++;
				}
			}
			System.out.println("matching count=" + retVal);
		}

		long estimatedTime = System.currentTimeMillis() - startTime;
		System.out.println("estimatedTime=" + estimatedTime + "ms");

		return retVal;
	}

}
