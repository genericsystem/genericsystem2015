package org.genericsystem.cv;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

public class ClassImgFieldsDetector extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "aligned-image-3.png";
	private final static String adjustedDirectoryPath2 = "aligned-image-3.png/all/image5-0";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;

		ImgClass imgClass = ImgClass.fromDirectory(classImgRepertory, false);
		mainGrid.add(buildImageViewFromMat(imgClass.getAverage()), columnIndex, rowIndex++);

		mainGrid.add(buildImageViewFromMat(imgClass.getVariance()), columnIndex, rowIndex++);

		Mat bgr = imgClass.getVariance();
		List<Mat> bgrChannels = new ArrayList<Mat>(Arrays.asList(new Mat(), new Mat(), new Mat()));
		Core.split(bgr, bgrChannels);

		for (Mat channel : bgrChannels)
			mainGrid.add(buildImageViewFromMat(channel), columnIndex, rowIndex++);

		ImgClass imgClass2 = ImgClass.fromDirectory(classImgRepertory, true);
		mainGrid.add(buildImageViewFromMat(imgClass2.getAverage()), columnIndex, rowIndex++);

		mainGrid.add(buildImageViewFromMat(imgClass2.getVariance()), columnIndex, rowIndex++);

		Mat hsv = imgClass2.getVariance();
		List<Mat> hsvChannels = new ArrayList<Mat>(Arrays.asList(new Mat(), new Mat(), new Mat()));
		Core.split(hsv, hsvChannels);

		// Core.subtract(hsvChannels.get(1), hsvChannels.get(0), hsvChannels.get(1));
		// Core.subtract(hsvChannels.get(2), hsvChannels.get(0), hsvChannels.get(2));

		for (Mat channel : hsvChannels)
			mainGrid.add(buildImageViewFromMat(channel), columnIndex, rowIndex++);

		// List<Mat> classMats = getClassMats(classImgRepertory);
		// Mat variance = getVariance(classMats);
		// mainGrid.add(buildImageViewFromMat(variance), columnIndex, rowIndex++);
		// Mat highlightedVariance = highlightVariance(variance);
		// // Imgproc.cvtColor(highlightedVariance, highlightedVariance, Imgproc.COLOR_BGR2GRAY);
		// mainGrid.add(buildImageViewFromMat(highlightedVariance), columnIndex, rowIndex++);
		//
		// List<Rect> zones = getRectZones(highlightedVariance);
		// List<Mat> bluredMats = getClassMats(adjustedDirectoryPath2);
		// for (Mat mat : bluredMats) {
		// List<String> ocrs = new ArrayList<>();
		// for (Rect rect : zones) {
		// String s = Ocr.doWork(new Mat(mat, rect).clone());
		// ocrs.add(s = s.replace("\n", "").trim());
		// System.out.println(s);
		// Imgproc.rectangle(mat, rect.tl(), rect.br(), new Scalar(0, 255, 0), 3);
		// // Imgproc.putText(mat, s, new Point(rect.tl().x, rect.br().y), Core.FONT_HERSHEY_PLAIN, 1.8, new Scalar(0, 0, 255), 2);
		// }
		// mainGrid.add(buildImageViewFromMat(mat), columnIndex, rowIndex);
		// VBox vbox = new VBox();
		// ocrs.forEach(ocr -> vbox.getChildren().add(new Label(ocr)));
		// mainGrid.add(vbox, columnIndex + 1, rowIndex++);
		// }
		columnIndex++;
	}

	private List<Mat> getClassMats(String repository) {
		return Arrays.stream(new File(repository).listFiles()).filter(img -> img.getName().endsWith(".png")).map(img -> Imgcodecs.imread(img.getPath())).collect(Collectors.toList());
	}

	private Mat getVariance(List<Mat> mats) {
		Mat average = adjust(mats.get(0));
		Mat nVariance = new Mat(average.size(), CvType.CV_32S, new Scalar(0, 0, 0));
		for (int n = 1; n < 10 * mats.size(); n++)
			computeImage(average, nVariance, adjust(mats.get(n % mats.size())), n + 1);
		return normalize(nVariance, mats.size());
	}

	private Mat normalize(Mat nVariance, int n) {
		Mat variance = new Mat();
		Core.multiply(nVariance, new Scalar(1 / Integer.valueOf(n).doubleValue()), variance);
		Core.convertScaleAbs(variance, variance);
		return variance;

	}

	private Mat highlightVariance(Mat variance) {
		Mat superVariance = new Mat();
		Imgproc.dilate(variance, superVariance, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(11, 3)));
		Imgproc.GaussianBlur(superVariance, superVariance, new Size(11, 3), 0);
		return superVariance;
	}

	private static void computeImage(Mat average, Mat nVariance, Mat adjusted, int n) {
		Mat mask = Mat.ones(nVariance.size(), CvType.CV_8U);
		Mat delta = new Mat(nVariance.size(), CvType.CV_32S);
		Core.subtract(adjusted, average, delta, mask, CvType.CV_32S);
		Core.addWeighted(average, 1, delta, 1 / Integer.valueOf(n).doubleValue(), 0, average, average.type());
		Mat delta2 = new Mat(nVariance.size(), CvType.CV_32S);
		Core.subtract(adjusted, average, delta2, mask, CvType.CV_32S);
		Mat product = delta.mul(delta2);
		Core.add(nVariance, product, nVariance);
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

	// private Mat equalizeHisto(Mat mat) {
	// Mat result = new Mat();
	// Imgproc.cvtColor(mat, result, Imgproc.COLOR_BGR2YCrCb);
	// List<Mat> channels = new ArrayList<Mat>();
	// Core.split(result, channels);
	// Imgproc.equalizeHist(channels.get(0), channels.get(0));
	// // Imgproc.equalizeHist(channels.get(1), channels.get(1));
	// // Imgproc.equalizeHist(channels.get(2), channels.get(2));
	// Core.merge(channels, result);
	// Imgproc.cvtColor(result, result, Imgproc.COLOR_YCrCb2BGR);
	// return result;
	// }
	//
	// private Mat prepareOcr(Mat mat) {
	// // Mat tmp = new Mat();
	// // Imgproc.blur(mat, tmp, new Size(3, 3));
	// // Mat tmp = equalizeHisto(mat);
	// // Mat tmp = Kmeans.colorMapKMeans(mat, 7);
	// // Mat tmp = new Mat();
	// // Imgproc.blur(tmp, tmp, new Size(3, 3));
	// // Imgproc.cvtColor(mat, tmp, Imgproc.COLOR_BGR2GRAY);
	// // / Imgproc.adaptiveThreshold(tmp, tmp, 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY, 11, 2);
	// // Imgproc.threshold(tmp, tmp, 0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
	// // Mat result = new Mat();
	// // Imgproc.cvtColor(tmp, tmp, Imgproc.COLOR_GRAY2BGR);
	// return mat;
	// }
}