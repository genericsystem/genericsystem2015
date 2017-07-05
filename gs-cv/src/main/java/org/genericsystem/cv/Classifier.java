package org.genericsystem.cv;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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

	public final static int MATCHING_THRESHOLD = 150;
	private static final String pngDirectoryPath = "png";
	private static final String classesDirectoryPath = "classes";

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		Path classesDirectory = Paths.get(classesDirectoryPath);
		Arrays.stream(new File(pngDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).forEach(imgFile -> classify(classesDirectory, imgFile.toPath()));
	}

	public static Mat compareFeature(String filename1, String filename2, int matching_threshold, int featureDetector, int descriptorExtractor) {
		Mat img1 = Imgcodecs.imread(filename1, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		Mat img2 = Imgcodecs.imread(filename2, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		CompareFeatureResult result = compareFeature(img1, img2, matching_threshold, featureDetector, descriptorExtractor);
		// if (result != null) {
		// String dir = alignedDirectoryPath + "-" + filename2.replaceFirst(".*/", "");
		// new File(dir).mkdirs();
		// Imgcodecs.imwrite(dir + "/" + filename1.replaceFirst(".*/", ""), result);
		// }
		return result != null ? result.getImg() : null;
	}

	// Stores the given image in the best class found in the given classesDirectory, creates a new class if necessary.
	// Returns the path to the stored image (the file name can have been changed to avoid duplicate names).
	public static Path classify(Path classesDirectory, Path imgFile) {
		Mat img = Imgcodecs.imread(imgFile.toString());
		CompareFeatureResult bestClass = Classifier.selectBestClass(classesDirectory, img);
		System.gc();
		System.runFinalization();
		Path matchingClassDir;
		Mat alignedImage = null;
		if (bestClass != null) {
			matchingClassDir = Paths.get(".").resolveSibling(bestClass.getImgClass().getDirectory());
			alignedImage = bestClass.getImg();
		} else {
			matchingClassDir = classesDirectory.resolve(System.nanoTime() + "");
			matchingClassDir.toFile().mkdirs();
			try {
				alignedImage = new Img(img).cropAndDeskew().getSrc();
			} catch (Exception e) {
				matchingClassDir.toFile().delete();
				System.out.println("Error while deskewing new image " + imgFile.toString() + " to create new class, new class not created.");
				e.printStackTrace();
				return null;
				// TODO: Store the image somewhere else.
			}
		}
		Path savedFile = matchingClassDir.resolve(imgFile.getFileName());
		try {
			synchronized (Classifier.class) {
				if (savedFile.toFile().exists()) {
					String[] fileNameParts = imgFile.getFileName().toString().split("\\.(?=[^\\.]+$)");
					savedFile = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], matchingClassDir.toFile()).toPath();
				}
			}
			Imgcodecs.imwrite(savedFile.toString(), alignedImage);
			return savedFile;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	public static CompareFeatureResult selectBestClass(Path classesDirectory, Mat img) {
		int[] matchingThresholds = new int[] { 30 };
		int[] featureDetectors = new int[] { FeatureDetector.BRISK };
		int[] descriptorExtractors = new int[] { DescriptorExtractor.OPPONENT_ORB };
		Map<String, List<CompareFeatureResult>> resultsPerClass = new HashMap<>();
		for (int i = 0; i < matchingThresholds.length; i++) {
			CompareFeatureResult algoResult = selectBestClass(classesDirectory, img, matchingThresholds[i], featureDetectors[i], descriptorExtractors[i]);
			if (algoResult != null) {
				String className = algoResult.getImgClass().getDirectory();
				List<CompareFeatureResult> classResults = resultsPerClass.get(className);
				if (classResults == null)
					classResults = new ArrayList<>();
				classResults.add(algoResult);
				resultsPerClass.put(className, classResults);
			}
			System.gc();
			System.runFinalization();
		}
		List<CompareFeatureResult> bestResults = new ArrayList<>();
		for (Entry<String, List<CompareFeatureResult>> entry : resultsPerClass.entrySet()) {
			List<CompareFeatureResult> results = entry.getValue();
			Collections.sort(results);
			if (results.size() > bestResults.size() || results.size() == bestResults.size() && results.get(0).getMatchingCount() > bestResults.get(0).getMatchingCount())
				bestResults = entry.getValue();
		}

		if (bestResults.size() < (matchingThresholds.length + 1) / 2)
			return null; // No class found

		System.out.println("Best results found: " + bestResults);
		return bestResults.get(0);
	}

	// Returns the best class for given algorithms and threshold.
	public static CompareFeatureResult selectBestClass(Path classesDirectory, Mat img, int matching_threshold, int featureDetector, int descriptorExtractor) {
		List<CompareFeatureResult> results = new ArrayList<>();
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(classesDirectory, Files::isDirectory)) {
			MatOfKeyPoint keypoints1 = getKeyPoints(img, featureDetector);
			Mat descriptors1 = getDescriptors(img, keypoints1, descriptorExtractor);
			for (Path path : directoryStream) {
				ImgClass imgClass = new ImgClass(path.toString());

				CompareFeatureResult classResult = Classifier.compareFeature(img, keypoints1, descriptors1, imgClass, matching_threshold, featureDetector, descriptorExtractor);
				if (classResult != null)
					results.add(classResult);
				System.gc();
				System.runFinalization();
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
		Collections.sort(results);
		return results.isEmpty() ? null : results.get(0);
	}

	public static Mat compareFeature(Mat img1, Mat img2, int matching_threshold) {
		CompareFeatureResult result = compareFeature(img1, img2, matching_threshold, FeatureDetector.PYRAMID_BRISK, DescriptorExtractor.OPPONENT_ORB);
		return result != null ? result.getImg() : null;
	}

	public static CompareFeatureResult compareFeature(Mat img1, MatOfKeyPoint keypoints1, Mat descriptors1, ImgClass imgClass, int matchingThreshold, int featureDetector, int descriptorExtractor) {
		CompareFeatureResult result = compareFeature(img1, keypoints1, descriptors1, imgClass.getClassModel() != null ? imgClass.getClassModel().getSrc() : imgClass.getMean().getSrc(), matchingThreshold, featureDetector, descriptorExtractor);
		if (result != null)
			result.setImgClass(imgClass);
		return result;
	}

	public static CompareFeatureResult compareFeature(Mat img1, Mat img2, int matchingThreshold, int featureDetector, int descriptorExtractor) {
		MatOfKeyPoint keypoints1 = getKeyPoints(img1, featureDetector);
		return compareFeature(img1, keypoints1, getDescriptors(img1, keypoints1, descriptorExtractor), img2, matchingThreshold, featureDetector, descriptorExtractor);
	}

	private static CompareFeatureResult compareFeature(Mat img1, MatOfKeyPoint keypoints1, Mat descriptors1, Mat img2, int matchingThreshold, int featureDetector, int descriptorExtractor) {
		// Declare key point of images
		MatOfKeyPoint keypoints2 = getKeyPoints(img2, featureDetector);
		Mat descriptors2 = getDescriptors(img2, keypoints2, descriptorExtractor);

		CompareFeatureResult result = null;
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

			// Extract good images (distances are under 10)
			List<DMatch> goodMatches = new ArrayList<>();
			for (int i = 0; i < descriptors1.rows(); i++) {
				if (match[i].distance <= 30) {
					goodMatches.add(match[i]);
				}
			}
			if (goodMatches.size() > matchingThreshold) {
				Mat imgMatches = new Mat();
				Features2d.drawMatches(img1, keypoints1, img2, keypoints2, new MatOfDMatch(goodMatches.stream().toArray(DMatch[]::new)), imgMatches);
				List<Point> objectPoints = new ArrayList<>();
				List<Point> scenePoints = new ArrayList<>();
				for (DMatch goodMatch : goodMatches) {
					objectPoints.add(keypoints1.toList().get(goodMatch.queryIdx).pt);
					scenePoints.add(keypoints2.toList().get(goodMatch.trainIdx).pt);
				}

				Mat homography = Calib3d.findHomography(new MatOfPoint2f(objectPoints.stream().toArray(Point[]::new)), new MatOfPoint2f(scenePoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
				Mat transformedImage = new Mat();
				Imgproc.warpPerspective(img1, transformedImage, homography, new Size(img2.cols(), img2.rows()));
				result = new CompareFeatureResult(transformedImage, goodMatches.size());
				System.out.println("----------------- possible match found, featureDetector: " + featureDetector + ", extractor: " + descriptorExtractor + ", threshold: " + matchingThreshold + ", goodMatches: " + goodMatches.size());
			} else
				System.out.println("----------------- not a match, featureDetector: " + featureDetector + ", extractor: " + descriptorExtractor + ", threshold: " + matchingThreshold + ", goodMatches: " + goodMatches.size());
		}
		return result;
	}

	public static MatOfKeyPoint getKeyPoints(Mat img, int featureDetector) {
		MatOfKeyPoint keypoints = new MatOfKeyPoint();
		FeatureDetector detector = FeatureDetector.create(featureDetector);
		detector.detect(img, keypoints);
		return keypoints;
	}

	public static Mat getDescriptors(Mat img, MatOfKeyPoint keypoints, int descriptorExtractor) {
		Mat descriptors = new Mat();
		DescriptorExtractor extractor = DescriptorExtractor.create(descriptorExtractor);
		extractor.compute(img, keypoints, descriptors);
		return descriptors;
	}

	public static class CompareFeatureResult implements Comparable<CompareFeatureResult> {
		private final Mat img;
		private ImgClass imgClass;
		private final int matchingCount;

		public CompareFeatureResult(Mat img, int matchingCount) {
			this.img = img;
			this.matchingCount = matchingCount;
		}

		public Mat getImg() {
			return img;
		}

		public ImgClass getImgClass() {
			return imgClass;
		}

		public int getMatchingCount() {
			return matchingCount;
		}

		public void setImgClass(ImgClass imgClass) {
			this.imgClass = imgClass;
		}

		// Decreasing order on matchingCount.
		@Override
		public int compareTo(CompareFeatureResult o) {
			return o.matchingCount - matchingCount;
		}

		@Override
		public String toString() {
			return "CompareFeatureResult, matchingCount: " + matchingCount + ", imgClass: " + imgClass.getDirectory();
		}
	}
}
