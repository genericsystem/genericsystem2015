package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class MotionDetector {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String adjustedDirectoryPath = "aligned-image-3.png";

	private static VideoCapture camera = new VideoCapture(0);
	private static List<Mat> adjustedImages = Arrays.stream(new File(adjustedDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).map(img -> Imgcodecs.imread(img.getPath())).collect(Collectors.toList());
	private static int i;

	static boolean read(Mat frame) {
		// return camera.read(frame);
		System.out.println(i + " " + i % adjustedImages.size());
		adjustedImages.get(i % adjustedImages.size()).copyTo(frame);
		// System.out.println("ZZZZZZZZZZZZZZ" + adjustedImages.get(i % adjustedImages.size()) + "========== " + frame);
		i++;

		return true;
	}

	public static void main(String[] args) {

		JFrame jframe = new JFrame("Motion Detector");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);

		Mat frame = new Mat();
		read(frame);
		jframe.setSize(frame.width(), frame.height());
		jframe.setVisible(true);
		// Mat prevAdjustedFrame = adjust(frame);
		Mat average = adjust(frame);
		double n = 2;
		Mat m2 = new Mat(average.size(), CvType.CV_32S, new Scalar(0, 0, 0));
		while (read(frame)) {
			Mat currentAdjustedFrame = adjust(frame);
			updateM2AndMean(m2, average, currentAdjustedFrame, n);
			Mat variance = new Mat();
			Core.multiply(m2, new Scalar(1 / n), variance);
			Core.convertScaleAbs(variance, variance);
			Mat bwVariance = new Mat();
			// Imgproc.threshold(variance, bwVariance, 100, 255, Imgproc.THRESH_TOZERO);
			Imgproc.dilate(variance, bwVariance, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(11, 3)));
			Imgproc.GaussianBlur(bwVariance, bwVariance, new Size(11, 3), 0);
			detection_contours(frame, bwVariance, n);
			ImageIcon image = new ImageIcon(mat2bufferedImage(frame));
			vidpanel.setIcon(image);
			vidpanel.repaint();
			n++;
		}
	}

	public static Mat adjust(Mat frame) {
		Mat mask = new Mat();
		Core.inRange(frame, new Scalar(0, 0, 0), new Scalar(80, 255, 255), mask);
		// Core.add(frame, new Scalar(0, 40, 0), frame);
		Mat masked = new Mat();
		frame.copyTo(masked, mask);
		Mat grey = new Mat();
		Imgproc.cvtColor(masked, grey, Imgproc.COLOR_BGR2GRAY);
		// Imgproc.adaptiveThreshold(grey, grey, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, 3, 0);
		// Imgproc.threshold(grey, grey, 160, 255, Imgproc.THRESH_TOZERO);
		// Imgproc.GaussianBlur(grey, grey, new Size(3, 3), 0);
		// Imgproc.dilate(grey, grey, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(3, 3)));
		return grey;
	}

	private static void updateM2AndMean(Mat m2, Mat mean, Mat newFrame, double n) {
		Mat mask = Mat.ones(m2.size(), CvType.CV_8U);
		Mat delta = new Mat(m2.size(), CvType.CV_32S);
		Core.subtract(newFrame, mean, delta, mask, CvType.CV_32S);
		Core.addWeighted(mean, 1, delta, 1 / n, 0, mean, mean.type());
		Mat delta2 = new Mat(m2.size(), CvType.CV_32S);
		Core.subtract(newFrame, mean, delta2, mask, CvType.CV_32S);
		Mat product = delta.mul(delta2);
		Core.add(m2, product, m2);
	}

	public static void detection_contours(Mat frame, Mat diffFrame, double n) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(diffFrame, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);

		double maxArea = 500;

		// contours = contours.stream().filter(c -> Imgproc.contourArea(c) >= maxArea).collect(Collectors.toList());
		// contours.sort((a, b) -> Double.compare(Imgproc.contourArea(b), Imgproc.contourArea(a)));
		// if (contours.size() > 0) {
		// rectangles.add(Imgproc.boundingRect(contours.get(0)));
		// Imgproc.drawContours(frame, contours, 0, new Scalar(0, 255, 0));
		// }

		Collections.sort(contours, (c1, c2) -> Double.compare(Imgproc.contourArea(c2), Imgproc.contourArea(c1)));
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > maxArea) {
				// maxArea = contourarea;
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				// Point[] result = new Point[4];
				// Imgproc.minAreaRect(contour2F).points(result);
				// Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 2);
				Rect rect = Imgproc.boundingRect(contour);

				if (n > 300) {
					Mat ocrZone = new Mat(frame, new Rect(rect.x, rect.y, rect.width, rect.height));
					// Imgproc.GaussianBlur(ocrZone, ocrZone, new Size(3, 3), 0);
					// Mat mask = new Mat();
					// Mat hsv = new Mat();

					// Imgproc.cvtColor(ocrZone, hsv, Imgproc.COLOR_BGR2HSV);
					// Core.inRange(hsv, new Scalar(0, 0, 0), new Scalar(255, 255, 100), mask);
					// Core.add(frame, new Scalar(0, 40, 0), frame);
					// Mat masked = new Mat();
					// ocrZone.copyTo(masked,mask);

					String ocrText = Ocr.doWork(App.createFileFromMat(ocrZone, rect.toString() + n + ".png", "ocr"));
					System.out.println(rect.toString() + n + " ====> " + ocrText);
				}
				Imgproc.drawContours(frame, contours, i, new Scalar(0, 255, 0), 2);
				Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(0, 0, 255), 1);

			}
		}
	}

	public static BufferedImage mat2bufferedImage(Mat image) {
		MatOfByte bytemat = new MatOfByte();
		Imgcodecs.imencode(".jpg", image, bytemat);
		try {
			return ImageIO.read(new ByteArrayInputStream(bytemat.toArray()));
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

	public static Mat computeDiffFrame(Mat currentAdjustedFrame, Mat prevAdjustedFrame) {
		Mat result = new Mat();
		Core.absdiff(currentAdjustedFrame, prevAdjustedFrame, result);
		// Imgproc.dilate(result, result, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(3, 3)));
		Imgproc.adaptiveThreshold(result, result, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 9, 7);
		return result;
	}

}