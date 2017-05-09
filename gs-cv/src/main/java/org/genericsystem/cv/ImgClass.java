package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
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
		Tools.getClassMats(directory).forEach(bgrImages::add);
		List<Mat> meanAndVariance = computeMeanAndVariance(Function.identity());
		average = meanAndVariance.get(0);
		variance = meanAndVariance.get(1);
	}

	public Mat computeMean(Function<Mat, Mat> filter) {
		Mat mean = filter.apply(bgrImages.get(0));
		for (int count = 1; count < bgrImages.size(); count++)
			Core.addWeighted(mean, 1d - 1d / Integer.valueOf(count).doubleValue(), filter.apply(bgrImages.get(count)), 1d / Integer.valueOf(count).doubleValue(), 0, mean);
		return mean;
	}

	public Mat computeBluredMean(Size blurSize) {
		return computeMean(mat -> {
			Mat result = new Mat();
			Imgproc.GaussianBlur(mat, result, blurSize, 0);
			return result;
		});
	}

	public Mat computeBluredVariance(Size blurSize) {
		return computeMeanAndVariance(mat -> {
			Mat result = new Mat();
			Imgproc.GaussianBlur(mat, result, blurSize, 0);
			return result;
		}).get(1);
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

	public List<Mat> computeMeanAndVariance(Function<Mat, Mat> filter) {
		int type = CvType.CV_32SC3;
		Mat img0 = filter.apply(bgrImages.get(0));
		Mat mean = new Mat(img0.size(), type, new Scalar(0, 0, 0));
		Mat m2 = new Mat(img0.size(), type, new Scalar(0, 0, 0));
		Mat mask = Mat.ones(img0.size(), CvType.CV_8U);
		int count = 1;
		for (; count <= bgrImages.size(); count++) {
			Mat img = new Mat();
			filter.apply(bgrImages.get(count - 1)).convertTo(img, type);
			;
			Mat delta = new Mat(img.size(), type);
			Core.subtract(img, mean, delta, mask, type);
			Core.addWeighted(mean, 1, delta, 1d / count, 0, mean, type);
			Mat delta2 = new Mat(m2.size(), type);
			Core.subtract(img, mean, delta2, mask, type);
			Mat product = delta.mul(delta2);
			Core.add(m2, product, m2);
		}
		Mat variance = new Mat(m2.size(), type);
		Core.multiply(m2, new Scalar(1d / count, 1d / count, 1d / count), variance);
		variance.convertTo(variance, CvType.CV_8UC3);
		mean.convertTo(mean, CvType.CV_8UC3);
		List<Mat> result = new ArrayList<>();
		result.add(mean);
		result.add(variance);
		return result;
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
		return computeMeanAndVariance(mat -> {
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
		}).get(1);
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
