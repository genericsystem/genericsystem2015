package org.genericsystem.cv;

import java.util.List;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.scene.layout.GridPane;

public class ClassImgFieldsDetector extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "aligned-image-3.png";

	// private final static String adjustedDirectoryPath2 = "aligned-image-3.png/mask/image-3";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;

		ImgClass imgClass = ImgClass.fromDirectory(null, classImgRepertory);
		Img model = imgClass.getMean();
		Img model2 = new Img(model.getSrc());
		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

		imgClass.addMapper(img -> img.range(new Scalar(0, 0, 0), new Scalar(80, 255, 255), new Scalar(0, 0, 0), false));
		imgClass.addMapper(img -> img.range(new Scalar(0, 0, 0), new Scalar(255, 255, 85), new Scalar(0, 0, 0), true));

		Img mean = imgClass.getMean().cvtColor(Imgproc.COLOR_BGR2GRAY).thresHold(1, 255, Imgproc.THRESH_BINARY);
		Img variance = imgClass.getVariance().cvtColor(Imgproc.COLOR_BGR2GRAY).thresHold(1, 255, Imgproc.THRESH_BINARY);

		mainGrid.add(mean.getImageView(), columnIndex, rowIndex++);
		mainGrid.add(variance.getImageView(), columnIndex, rowIndex++);

		mean = mean.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, new Size(7, 3)));
		variance = variance.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, new Size(7, 3)));

		mean = mean.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(25, 11)));
		variance = variance.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(25, 11)));

		mainGrid.add(mean.getImageView(), columnIndex, rowIndex++);
		mainGrid.add(variance.getImageView(), columnIndex, rowIndex++);

		ImgZoner.drawZones(mean, model, 400, new Scalar(0, 255, 0), 3);
		mainGrid.add(model.getImageView(), columnIndex, rowIndex++);

		ImgZoner.drawZones(variance, model2, 400, new Scalar(0, 255, 0), 3);
		mainGrid.add(model2.getImageView(), columnIndex, rowIndex++);

		// imgClass.addMapper(img -> img.thresHold(5, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY));
		// mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);
		//
		// imgClass.addMapper(img -> img.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, new Size(25, 5))));
		// mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);
		//

		// imgClass.addMapper(img -> img.thresHold(5, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY));
		// // ImgClass imgClass = ImgClass.fromDirectory(classImgRepertory);
		// double dx = 5;
		// double dy = 5;
		// Mat imageToZone = highlight(imgClass.computeRangedMean(new Scalar(0, 0, 0), new Scalar(255, 255, 90), true, false), new Scalar(20, 20, 20));
		// // ImgZoner.drawZones(imageToZone, 1, new Scalar(255, 255, 255), -1);
		// // ImgZoner.drawAdjustedZones(imageToZone, dx, dy, new Scalar(0, 255, 0), 3);
		// mainGrid.add(buildImageViewFromMat(imageToZone), columnIndex, rowIndex++);
		//
		// imgClass2.addMapper(img -> img.cvtColor(Imgproc.COLOR_BGR2GRAY).thresHold(0, 255, Imgproc.THRESH_BINARY).cvtColor(Imgproc.COLOR_GRAY2BGR));
		// imgClass2.addMapper(img -> img.range(new Scalar(0, 0, 0), new Scalar(255, 255, 5), true));
		// imgClass.addMapper(img -> img.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 5))));
		// imgClass2.addMapper(img -> img.gaussianBlur(new Size(15, 15)));
		// mainGrid.add(imgClass2.getMean().getImageView(), columnIndex, rowIndex++);
		// img = img.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(3, 3)));

		// imgClass.addMapper(img -> img.gaussianBlur(new Size(3, 3)));
		// Img im = new Img(imgClass.computeRangedVariance(new Scalar(0, 0, 0), new Scalar(255, 255, 90), true));
		// mainGrid.add(buildImageViewFromMat(im.getSrc()), columnIndex, rowIndex++);
		// im = im.cvtColor(Imgproc.COLOR_BGR2GRAY);
		// im = im.thresHold(5, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		// im = im.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 5)));
		//
		// // TextDetectors.lpdDetectText(im);
		// // ImgZoner.drawZones(im.getSrc(), 300, new Scalar(255), -1);
		// // mainGrid.add(buildImageViewFromMat(imageToZone2), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(im.getSrc()), columnIndex, rowIndex++);
		//
		// // mainGrid.add(buildImageViewFromMat(imgClass.getAverage()), columnIndex, rowIndex++);
		// // mainGrid.add(buildImageViewFromMat(imgClass.getVariance()), columnIndex, rowIndex++);
		//
		// Mat bluredVariance = imgClass.computeBluredVariance(new Size(15, 15));
		// Imgproc.cvtColor(bluredVariance, bluredVariance, Imgproc.COLOR_BGR2HSV);
		// Mat mask = new Mat();
		// Core.inRange(bluredVariance, new Scalar(0, 0, 0), new Scalar(255, 1, 255), mask);
		// Mat result = new Mat(bluredVariance.size(), bluredVariance.type(), new Scalar(0, 0, 0));
		// bluredVariance.copyTo(result, mask);
		// Imgproc.cvtColor(result, result, Imgproc.COLOR_HSV2BGR);
		// // Imgproc.dilate(result, result, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(17, 3)));
		// // Imgproc.GaussianBlur(result, result, new Size(17, 3), 0);
		// // ImgZoner.drawAdjustedZones(result, 500, dx, dy, new Scalar(0, 255, 0), 3);
		// mainGrid.add(buildImageViewFromMat(result), columnIndex, rowIndex++);
		//
		// Mat bluredVariance2 = imgClass.computeBluredVariance(new Size(31, 31));
		// Imgproc.cvtColor(bluredVariance2, bluredVariance2, Imgproc.COLOR_BGR2HSV);
		// Mat mask2 = new Mat();
		// Core.inRange(bluredVariance2, new Scalar(0, 0, 0), new Scalar(255, 1, 255), mask2);
		// Mat result2 = new Mat(bluredVariance2.size(), bluredVariance2.type(), new Scalar(0, 0, 0));
		// bluredVariance2.copyTo(result2, mask2);
		// Imgproc.cvtColor(result2, result2, Imgproc.COLOR_HSV2BGR);
		// // Imgproc.dilate(result2, result2, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(17, 3)));
		// // Imgproc.GaussianBlur(result2, result2, new Size(17, 3), 0);
		// // ImgZoner.drawAdjustedZones(result2, 500, dx, dy, new Scalar(0, 255, 0), 3);
		// mainGrid.add(buildImageViewFromMat(result2), columnIndex, rowIndex++);

		// mainGrid.add(buildImageViewFromMat(imgClass.computeBluredVariance(new Size(31, 31))), columnIndex, rowIndex++);
		//
		// // mainGrid.add(buildImageViewFromMat(imgClass.computeRangedMean(new Scalar(0, 0, 0), new Scalar(220, 180, 230), true, true)), columnIndex, rowIndex++);
		// // mainGrid.add(buildImageViewFromMat(imgClass.computeRangedVariance(new Scalar(0, 0, 0), new Scalar(255, 255, 60), true)), columnIndex, rowIndex++);
		// // mainGrid.add(buildImageViewFromMat(highlight(imgClass.computeRangedVariance(new Scalar(0, 0, 0), new Scalar(255, 255, 60), true), 40)), columnIndex, rowIndex++);
		//
		// mainGrid.add(buildImageViewFromMat(Kmeans.colorMapKMeans(imgClass.getAverage(), 4)), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(Kmeans.colorMapKMeans(imgClass.getVariance(), 4)), columnIndex, rowIndex++);
		//
		// List<Mat> clusters = Kmeans.cluster(imgClass.getAverage(), 4);
		// mainGrid.add(buildImageViewFromMat(clusters.get(0)), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(clusters.get(1)), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(clusters.get(2)), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(clusters.get(3)), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(clusters.get(4)), columnIndex, rowIndex++);
		// mainGrid.add(buildImageViewFromMat(clusters.get(5)), columnIndex, rowIndex++);

		// Mat img = Imgcodecs.imread("aligned-image-3.png/image-3.png");
		// Mat canny = new Mat();
		// Imgproc.cvtColor(img, img, Imgproc.COLOR_BGR2GRAY);
		// Imgproc.Canny(img, canny, canyThreshold1, canyThreshold2);
		// mainGrid.add(buildImageViewFromMat(canny), columnIndex, rowIndex++);

		// // highlight(imgClass.computeRangedMean(new Scalar(220, 0, 0), new Scalar(240, 180, 230), true, true), 1)
		//
		// List<Mat> sameMats = Tools.getClassMats(/* "aligned-image-3.png/mask/image-3" */ "aligned-image-3.png/foreground/image-3");
		// sameMats.addAll(Tools.getImages("aligned-image-3.png", "image-3.png"));
		// List<String> bestOcrs = new ArrayList<>();
		//
		// Map<Zone, Double> scores = new HashMap<>();
		// for (Zone zone : zones) {
		// Zone adjusted = zone.adjustRect(dx, dy, sameMats.get(0).width(), sameMats.get(0).height());
		// String scoredText = adjusted.computeUnsupervisedScoredText(sameMats);
		// scores.put(zone, adjusted.computeUnsupervisedScore(sameMats));
		// bestOcrs.add(scoredText);
		// System.out.println(scoredText);
		// }
		//
		// Mat scoredMat = sameMats.get(sameMats.size() - 1).clone();
		//
		// for (Zone zone : zones) {
		// Zone adjusted = zone.adjustRect(dx, dy, sameMats.get(0).width(), sameMats.get(0).height());
		// adjusted.draw(scoredMat, new Scalar(0, 255, 0), 1);
		// adjusted.write(scoredMat, "" + Math.floor((scores.get(zone) * 10000)) / 100 + "%", 1.8, new Scalar(0, 0, 255), 2);
		// }
		//
		// mainGrid.add(buildImageViewFromMat(scoredMat), columnIndex, rowIndex++);
		//
		// VBox vbox = new VBox();
		// bestOcrs.forEach(ocr -> vbox.getChildren().add(new Label(ocr)));
		// mainGrid.add(vbox, columnIndex, rowIndex++);
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

	private Mat highlight(Mat variance, Scalar scalar) {
		Mat superVariance = new Mat();
		Core.multiply(variance, scalar, superVariance);
		// Imgproc.dilate(superVariance, superVariance, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(17, 3)));
		// Imgproc.GaussianBlur(superVariance, superVariance, new Size(17, 3), 0);
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

	// public static List<Rect> getRectZones(Mat highlightVariance) {
	// // To improve
	// List<Rect> result = new ArrayList<>();
	// List<MatOfPoint> contours = new ArrayList<>();
	// Imgproc.findContours(highlightVariance, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
	// double minArea = 500;
	// Collections.sort(contours, (c1, c2) -> Double.compare(Imgproc.contourArea(c2), Imgproc.contourArea(c1)));
	// for (int i = 0; i < contours.size(); i++) {
	// MatOfPoint contour = contours.get(i);
	// double contourarea = Imgproc.contourArea(contour);
	// if (contourarea > minArea)
	// result.add(Imgproc.boundingRect(contour));
	//
	// }
	// return result;
	// }

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