package org.genericsystem.cv;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

public class ImgClass {

	private boolean hsv;
	private Mat average;
	private Mat variance;
	private final String directory;
	private final List<Mat> images = new ArrayList<>();

	public static ImgClass fromDirectory(String directory, boolean hsv) {
		return new ImgClass(directory, hsv);
	}

	public ImgClass(String directory, boolean hsv) {
		this.directory = directory;
		this.hsv = hsv;
		List<Mat> classMats = getClassMats(directory);
		classMats.stream().forEach(this::addImage);
	}

	private void addImage(Mat mat) {
		if (hsv)
			mat = maskHsv(mat);
		else
			mat = maskBgr(mat);
		images.add(mat);
		computeMeanVariance(mat);
	}

	private void computeMeanVariance(Mat mat) {
		double n = Integer.valueOf(images.size()).doubleValue();
		average = null;
		for (Mat img : images) {
			if (average == null)
				average = img;
			else
				Core.addWeighted(average, 1d - 1d / n, img, 1d / n, 0, average);
		}
		variance = null;
		for (Mat img : images) {
			if (variance == null)
				variance = new Mat(img.size(), img.type(), new Scalar(0, 0, 0));
			else {
				Mat diff = new Mat();
				Core.absdiff(average, img, diff);
				Core.addWeighted(variance, 1d - 1d / n, diff, 1d / n, 0, variance);
			}
		}
		Core.multiply(variance, new Scalar(10, 10, 10), variance);
	}

	public static Mat maskBgr(Mat frame) {
		Mat mask = new Mat();
		Mat result = new Mat();
		List<Mat> bgrChannels = new ArrayList<Mat>(Arrays.asList(new Mat(), new Mat(), new Mat()));
		Core.split(frame, bgrChannels);
		Core.inRange(bgrChannels.get(0), new Scalar(0), new Scalar(80), mask);
		Mat channel = new Mat(bgrChannels.get(0).size(), bgrChannels.get(0).type(), new Scalar(180));
		bgrChannels.get(0).copyTo(channel, mask);
		bgrChannels.set(0, channel);
		Core.merge(bgrChannels, result);
		// frame.copyTo(result, mask);
		return channel;
	}

	public static Mat maskHsv(Mat frame) {
		Mat mask = new Mat();
		Core.inRange(frame, new Scalar(0, 0, 0), new Scalar(255, 255, 100), mask);
		Mat result = new Mat();
		// Core.bitwise_and(frame, mask, result);
		frame.copyTo(result, mask);
		return result;
	}

	private List<Mat> getClassMats(String repository) {
		List<Mat> result = Arrays.stream(new File(repository).listFiles()).filter(img -> img.getName().endsWith(".png")).map(img -> Imgcodecs.imread(img.getPath())).collect(Collectors.toList());

		if (hsv)
			for (Mat mat : result)
				Imgproc.cvtColor(mat, mat, Imgproc.COLOR_BGR2HSV);
		return result;

	}

	public static Mat adjust(Mat frame) {
		Mat mask = new Mat();
		Core.inRange(frame, new Scalar(0, 0, 0), new Scalar(80, 255, 255), mask);
		Mat masked = new Mat();
		frame.copyTo(masked, mask);
		Mat grey = new Mat();
		Imgproc.cvtColor(masked, grey, Imgproc.COLOR_BGR2GRAY);
		return grey;
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

	public List<Mat> getImages() {
		return images;
	}

	public Mat getAverage() {
		return average;
	}

	public Mat getVariance() {
		return variance;
	}

}
