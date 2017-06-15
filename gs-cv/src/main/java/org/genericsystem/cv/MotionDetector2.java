package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.text.OCRTesseract;
import org.opencv.videoio.VideoCapture;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class MotionDetector2 extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private final ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private final OCRTesseract ocr = OCRTesseract.create("/usr/share/tesseract-ocr/4.00/", "fra", "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,<'", 1, 7);

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		capture.read(frame);
		Img average = adjust(frame);
		ImageView src = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src, 0, 0);
		double n = 20;
		timer.scheduleAtFixedRate(() -> {
			capture.read(frame);
			Img adjustedFrame = adjust(frame);
			Core.addWeighted(average.getSrc(), (n - 1) / n, adjustedFrame.getSrc(), 1d / n, 0, average.getSrc());
			Mat diffFrame = computeDiffFrame(adjustedFrame.getSrc(), average.getSrc());
			detection_contours(frame, diffFrame);
			// for (Rect rect : detection_contours(frame, diffFrame))
			// Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(0, 0, 255), 1);

			src.setImage(Tools.mat2jfxImage(frame));
		}, 0, 100, TimeUnit.MILLISECONDS);
	}

	public static Mat computeDiffFrame(Mat currentAdjustedFrame, Mat prevAdjustedFrame) {
		Mat result = new Mat();
		Core.absdiff(currentAdjustedFrame, prevAdjustedFrame, result);
		Imgproc.adaptiveThreshold(currentAdjustedFrame, result, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 17, 5);
		return result;
	}

	public List<Rect> detection_contours(Mat frame, Mat diffFrame) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(diffFrame, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);

		double minArea = 100;
		double maxArea = 2000;
		List<Rect> rectangles = new ArrayList<>();
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double area = Imgproc.contourArea(contour);
			if (area > minArea) {
				// maxArea = contourarea;
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				Point[] result = new Point[4];
				RotatedRect rect = Imgproc.minAreaRect(contour2F);
				rect.points(result);
				// if (rect.size.height * rect.size.width < maxArea) {
				Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 2);
				// if (rect.angle < -45.) {
				// rect.angle += 90.0;
				// double tmp = rect.size.width;
				// rect.size.width = rect.size.height;
				// rect.size.height = tmp;
				// }
				//
				// Mat matrix = Imgproc.getRotationMatrix2D(rect.center, rect.angle, 1);
				// Mat rotated = new Mat();
				// Mat res = new Mat();
				// Imgproc.warpAffine(frame, rotated, matrix, frame.size());
				// Imgproc.getRectSubPix(rotated, rect.size, rect.center, res);
				// System.out.println(ocr.run(res, 90, 1));
				// // rectangles.add(Imgproc.boundingRect(contour));
				// Imgproc.drawContours(frame, contours, i, new Scalar(0, 255, 0));
			}
		}
		return rectangles;

	}

	public Img adjust(Mat frame) {
		return new Img(frame).cvtColor(Imgproc.COLOR_BGR2GRAY).gaussianBlur(new Size(5, 5));
	}

	@Override
	public void stop() throws Exception {
		timer.shutdown();
		capture.release();
		super.stop();
	}
}