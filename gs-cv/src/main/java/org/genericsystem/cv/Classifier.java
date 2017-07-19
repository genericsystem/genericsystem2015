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
	private static final FeatureDetector[] featureDetectors;
	private static final DescriptorExtractor[] descriptorExtractors;

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		featureDetectors = new FeatureDetector[] { FeatureDetector.create(FeatureDetector.BRISK) };
		descriptorExtractors = new DescriptorExtractor[] { DescriptorExtractor.create(DescriptorExtractor.OPPONENT_ORB) };
	}

	public static void main(String[] args) {
		Path classesDirectory = Paths.get(classesDirectoryPath);
		Arrays.stream(new File(pngDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).forEach(imgFile -> classify(classesDirectory, imgFile.toPath(), featureDetectors, descriptorExtractors));
	}

	public static Mat compareFeature(String filename1, String filename2, int matching_threshold, FeatureDetector featureDetector, DescriptorExtractor descriptorExtractor) {
		Mat img1 = Imgcodecs.imread(filename1, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		Mat img2 = Imgcodecs.imread(filename2, Imgcodecs.CV_LOAD_IMAGE_COLOR);
		CompareFeatureResult result = compareFeature(img1, img2, matching_threshold, featureDetector, descriptorExtractor);
		// if (result != null) {
		// String dir = alignedDirectoryPath + "-" + filename2.replaceFirst(".*/", "");
		// new File(dir).mkdirs();
		// Imgcodecs.imwrite(dir + "/" + filename1.replaceFirst(".*/", ""), result);
		// }
		img1.release();
		img2.release();
		return result != null ? result.getImg() : null;
	}

	public static Path classify(Path classesDirectory, Path imgFile) {
		return classify(classesDirectory, imgFile, featureDetectors, descriptorExtractors);
	}

	// Stores the given image in the best class found in the given classesDirectory, creates a new class if necessary.
	// Returns the path to the stored image (the file name can have been changed to avoid duplicate names).
	public static Path classify(Path classesDirectory, Path imgFile, FeatureDetector[] featureDetectors, DescriptorExtractor[] descriptorExtractors) {
		Mat alignedImage = null;
		try (Img img = new Img(imgFile.toString());
				CompareFeatureResult bestClass = Classifier.selectBestClass(classesDirectory, img.getSrc(), featureDetectors, descriptorExtractors)) {
			Path matchingClassDir;
			if (bestClass != null) {
				System.out.println("bestClass != null, " + bestClass);
				matchingClassDir = Paths.get(".").resolveSibling(bestClass.getImgClass().getDirectory());
				alignedImage = bestClass.getImg();
			} else {
				matchingClassDir = classesDirectory.resolve(System.nanoTime() + "");
				matchingClassDir.toFile().mkdirs();
				try (Img cropped = img.cropAndDeskew()) {
					alignedImage = cropped.getSrc();
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
				System.out.println("alignedImage : " + alignedImage + ", path : " + savedFile);
				Imgcodecs.imwrite(savedFile.toString(), alignedImage);
				return savedFile;
			} catch (IOException e) {
				e.printStackTrace();
				return null;
			}
		} finally {
			if (alignedImage != null)
				alignedImage.release();
		}
	}

	public static CompareFeatureResult selectBestClass(Path classesDirectory, Mat img, FeatureDetector[] featureDetectors, DescriptorExtractor[] descriptorExtractors) {
		int[] matchingThresholds = new int[] { 30 };
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
		}
		List<CompareFeatureResult> bestResults = new ArrayList<>();
		for (Entry<String, List<CompareFeatureResult>> entry : resultsPerClass.entrySet()) {
			List<CompareFeatureResult> results = entry.getValue();
			Collections.sort(results);
			if (results.size() > bestResults.size() || results.size() == bestResults.size() && results.get(0).getMatchingCount() > bestResults.get(0).getMatchingCount())
				bestResults = entry.getValue();
		}

		List<CompareFeatureResult> bestResults_ = bestResults;
		resultsPerClass.values().forEach(list -> {
			if (list != bestResults_)
				list.forEach(c -> c.close());
		});
		bestResults.subList(1, bestResults.size()).forEach(c -> c.close());

		if (bestResults.size() < (matchingThresholds.length + 1) / 2)
			return null; // No class found

		return bestResults.get(0);
	}

	// Returns the best class for given algorithms and threshold.
	public static CompareFeatureResult selectBestClass(Path classesDirectory, Mat img, int matching_threshold, FeatureDetector detector, DescriptorExtractor extractor) {
		CompareFeatureResult result = null;
		MatOfKeyPoint keypoints1 = null;
		Mat descriptors1 = null;
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(classesDirectory, Files::isDirectory)) {
			keypoints1 = getKeyPoints(img, detector);
			descriptors1 = getDescriptors(img, keypoints1, extractor);
			for (Path path : directoryStream) {
				try (ImgClass imgClass = new ImgClass(path.toString())) {
					CompareFeatureResult classResult = Classifier.compareFeature(img, keypoints1, descriptors1, imgClass, matching_threshold, detector, extractor);
					if (classResult != null)
						if (result == null)
							result = classResult;
						else if	(classResult.compareTo(result) < 0) {
							result.close();
							result = classResult;
						} else
							classResult.close();
				}
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		} finally {
			keypoints1.release();
			descriptors1.release();
		}
		return result;
	}

	public static Mat compareFeature(Mat img1, Mat img2, int matching_threshold) {
		CompareFeatureResult result = compareFeature(img1, img2, matching_threshold, FeatureDetector.create(FeatureDetector.PYRAMID_BRISK), DescriptorExtractor.create(DescriptorExtractor.OPPONENT_ORB));
		return result != null ? result.getImg() : null;
	}

	public static CompareFeatureResult compareFeature(Mat img1, MatOfKeyPoint keypoints1, Mat descriptors1, ImgClass imgClass, int matchingThreshold, FeatureDetector featureDetector, DescriptorExtractor descriptorExtractor) {
		CompareFeatureResult result = compareFeature(img1, keypoints1, descriptors1, imgClass.getClassModel() != null ? imgClass.getClassModel().getSrc() : imgClass.getMean().getSrc(), matchingThreshold, featureDetector, descriptorExtractor);
		if (result != null)
			result.setImgClass(imgClass);
		return result;
	}

	public static CompareFeatureResult compareFeature(Mat img1, Mat img2, int matchingThreshold, FeatureDetector featureDetector, DescriptorExtractor descriptorExtractor) {
		MatOfKeyPoint keypoints1 = getKeyPoints(img1, featureDetector);
		Mat descriptors1 = getDescriptors(img1, keypoints1, descriptorExtractor);
		CompareFeatureResult result = compareFeature(img1, keypoints1, descriptors1, img2, matchingThreshold, featureDetector, descriptorExtractor);
		keypoints1.release();
		descriptors1.release();
		return result;
	}

	private static CompareFeatureResult compareFeature(Mat img1, MatOfKeyPoint keypoints1, Mat descriptors1, Mat img2, int matchingThreshold, FeatureDetector featureDetector, DescriptorExtractor descriptorExtractor) {
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
			matches.release();
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
				MatOfDMatch matOfDMatch = new MatOfDMatch(goodMatches.stream().toArray(DMatch[]::new));
				Features2d.drawMatches(img1, keypoints1, img2, keypoints2, matOfDMatch, imgMatches);
				imgMatches.release();
				matOfDMatch.release();
				List<Point> objectPoints = new ArrayList<>();
				List<Point> scenePoints = new ArrayList<>();
				for (DMatch goodMatch : goodMatches) {
					objectPoints.add(keypoints1.toList().get(goodMatch.queryIdx).pt);
					scenePoints.add(keypoints2.toList().get(goodMatch.trainIdx).pt);
				}

				MatOfPoint2f objectPointsMat = new MatOfPoint2f(objectPoints.stream().toArray(Point[]::new));
				MatOfPoint2f scenePointsMat = new MatOfPoint2f(scenePoints.stream().toArray(Point[]::new));
				Mat homography = Calib3d.findHomography(objectPointsMat, scenePointsMat, Calib3d.RANSAC, 10);
				objectPointsMat.release();
				scenePointsMat.release();
				Mat transformedImage = new Mat();
				Imgproc.warpPerspective(img1, transformedImage, homography, new Size(img2.cols(), img2.rows()));
				result = new CompareFeatureResult(transformedImage, goodMatches.size());
				homography.release();
				System.out.println("----------------- possible match found, threshold: " + matchingThreshold + ", goodMatches: " + goodMatches.size());
			} else
				System.out.println("----------------- not a match, threshold: " + matchingThreshold + ", goodMatches: " + goodMatches.size());
		}
		keypoints2.release();
		descriptors2.release();
		return result;
	}

	public static MatOfKeyPoint getKeyPoints(Mat img, FeatureDetector detector) {
		MatOfKeyPoint keypoints = new MatOfKeyPoint();
		detector.detect(img, keypoints);
		return keypoints;
	}

	public static Mat getDescriptors(Mat img, MatOfKeyPoint keypoints, DescriptorExtractor extractor) {
		Mat descriptors = new Mat();
		extractor.compute(img, keypoints, descriptors);
		return descriptors;
	}

	public static class CompareFeatureResult implements Comparable<CompareFeatureResult>, AutoCloseable {
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

		@Override
		public void close() {
			img.release();
			imgClass.close();
		}
	}
}
