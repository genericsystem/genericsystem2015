package org.genericsystem.cv;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

public class ImgClass {

	private Mat average;
	private Mat variance;
	private final String directory;
	private final List<Mat> bgrImages = new ArrayList<>();

	public static ImgClass fromDirectory(String bgrDirectory) {
		return new ImgClass(bgrDirectory);
	}

	public ImgClass(String bgrDirectory) {
		this.directory = bgrDirectory;
		getClassMats(directory).forEach(bgrImages::add);
		average = computeMean(Function.identity());
		variance = computeVariance(average, Function.identity());
	}

	public Mat computeMean(Function<Mat, Mat> filter) {
		Mat mean = filter.apply(bgrImages.get(0));
		for (int count = 1; count < bgrImages.size(); count++)
			Core.addWeighted(mean, 1d - 1d / Integer.valueOf(count).doubleValue(), filter.apply(bgrImages.get(count)), 1d / Integer.valueOf(count).doubleValue(), 0, mean);
		return mean;
	}

	public Mat computeVariance(Function<Mat, Mat> filter) {
		return computeVariance(computeMean(filter), filter);
	}

	public Mat computeBluredMean(Size blurSize) {
		return computeMean(mat -> {
			Mat result = new Mat();
			Imgproc.GaussianBlur(mat, result, blurSize, 0);
			return result;
		});
	}

	public Mat computeBluredVariance(Size blurSize) {
		return computeVariance(mat -> {
			Mat result = new Mat();
			Imgproc.GaussianBlur(mat, result, blurSize, 0);
			return result;
		});
	}

	public Mat computeVariance(Mat mean, Function<Mat, Mat> filter) {
		Mat diff = new Mat();
		Core.absdiff(mean, filter.apply(bgrImages.get(0)), diff);
		Mat variance = new Mat(mean.size(), mean.type(), new Scalar(0, 0, 0));
		for (int count = 1; count < bgrImages.size(); count++) {
			diff = new Mat();
			Core.absdiff(mean, filter.apply(bgrImages.get(count)), diff);
			Core.addWeighted(variance, 1d - 1d / Integer.valueOf(count).doubleValue(), diff, 1d / Integer.valueOf(count).doubleValue(), 0, variance);
		}
		return variance;
	}

	public Mat computeRangedMean(Scalar bgr1, Scalar bgr2, boolean hsv, boolean reverse) {
		return computeMean(mat -> {
			Mat ranged = new Mat();
			if (hsv)
				Imgproc.cvtColor(mat, ranged, Imgproc.COLOR_BGR2HSV);
			else
				mat.copyTo(ranged);
			Mat mask = new Mat();
			Core.inRange(ranged, bgr1, bgr2, mask);
			if (reverse)
				Core.bitwise_not(mask, mask);

			Mat result = new Mat(ranged.size(), ranged.type(), new Scalar(0, 0, 0));
			ranged.copyTo(result, mask);
			if (hsv)
				Imgproc.cvtColor(result, result, Imgproc.COLOR_HSV2BGR);
			return result;
		});
	}

	public Mat computeRangedVariance(Scalar bgr1, Scalar bgr2, boolean hsv) {
		return computeVariance(mat -> {
			Mat ranged = new Mat();
			if (hsv)
				Imgproc.cvtColor(mat, ranged, Imgproc.COLOR_BGR2HSV);
			else
				mat.copyTo(ranged);
			Mat mask = new Mat();
			Core.inRange(ranged, bgr1, bgr2, mask);
			Mat result = new Mat();
			ranged.copyTo(result, mask);
			if (hsv)
				Imgproc.cvtColor(result, result, Imgproc.COLOR_HSV2BGR);
			return result;
		});
	}

	private List<Mat> getClassMats(String repository) {
		return Arrays.stream(new File(repository).listFiles()).filter(img -> img.getName().endsWith(".png")).peek(img -> System.out.println("load : " + img.getPath())).map(img -> Imgcodecs.imread(img.getPath())).collect(Collectors.toList());
	}

	public static List<Rect> getRectZones(Mat highlightVariance) {
		// To improve
		List<Rect> result = new ArrayList<>();
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(highlightVariance, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 500;
		Collections.sort(contours, (c1, c2) -> Double.compare(Imgproc.contourArea(c2), Imgproc.contourArea(c1)));
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > minArea)
				result.add(Imgproc.boundingRect(contour));

		}
		return result;
	}

	public String getDirectory() {
		return directory;
	}

	public List<Mat> getBgrImages() {
		return bgrImages;
	}

	public Mat getAverage() {
		return average;
	}

	public Mat getVariance() {
		return variance;
	}

	// public Mat getGrayAverage() {
	// Mat greyVariance = new Mat();
	// if (hsv)
	// Imgproc.cvtColor(average, greyVariance, Imgproc.COLOR_HSV2BGR);
	// else
	// greyVariance = average.clone();
	// Imgproc.cvtColor(greyVariance, greyVariance, Imgproc.COLOR_BGR2GRAY);
	// return greyVariance;
	// }

	// public Mat getGrayVariance() {
	// Mat greyVariance = new Mat();
	// if (hsv)
	// Imgproc.cvtColor(variance, greyVariance, Imgproc.COLOR_HSV2BGR);
	// else
	// greyVariance = variance.clone();
	// Imgproc.cvtColor(greyVariance, greyVariance, Imgproc.COLOR_BGR2GRAY);
	// return greyVariance;
	// }

}
