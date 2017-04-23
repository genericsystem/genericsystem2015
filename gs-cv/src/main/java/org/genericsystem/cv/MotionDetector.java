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

	private final static String adjustedDirectoryPath = "adjusted";

	private static VideoCapture camera = new VideoCapture(0);
	private static List<Mat> adjustedImages = Arrays.stream(new File(adjustedDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).map(img -> Imgcodecs.imread(img.getPath())).peek(img -> Imgproc.resize(img, img, new Size(400d, 300d)))
			.collect(Collectors.toList());
	private static int i;

	static boolean read(Mat frame) {
		// return camera.read(frame);
		System.out.println(i + " " + i % adjustedImages.size());
		adjustedImages.get(i % adjustedImages.size()).copyTo(frame);
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
		Mat prevAdjustedFrame = adjust(frame);
		Mat average = prevAdjustedFrame;
		double n = 2;
		Mat diffAverage = new Mat(prevAdjustedFrame.size(), prevAdjustedFrame.type(), new Scalar(0, 0, 0));
		while (read(frame)) {
			Mat currentAdjustedFrame = adjust(frame);
			Core.addWeighted(average, (n - 1) / n, currentAdjustedFrame, 2d / n, 0, average);
			Mat diffFrame = computeDiffFrame(currentAdjustedFrame, average);

			Core.addWeighted(diffAverage, (n - 1) / n, diffFrame, 3d / n, -(n - 3) / (2 * n), diffAverage);
			Imgproc.threshold(diffAverage, diffAverage, 0, 255, Imgproc.THRESH_TOZERO);
			n++;
			// Imgproc.adaptiveThreshold(diffAverage, diffAverage, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, 9, 7);
			prevAdjustedFrame = currentAdjustedFrame;
			detection_contours(frame, diffAverage);
			ImageIcon image = new ImageIcon(mat2bufferedImage(frame));

			vidpanel.setIcon(image);
			vidpanel.repaint();
		}
	}

	private static Mat gradient(Mat src_gray) {
		Mat grad_x = new Mat();
		Mat grad_y = new Mat();
		Mat abs_grad_x = new Mat();
		Mat abs_grad_y = new Mat();

		int scale = 1;
		int delta = 0;
		int ddepth = CvType.CV_32FC1;

		// Calculate the x and y gradients using Sobel operator
		Imgproc.Sobel(src_gray, grad_x, ddepth, 1, 0, 3, scale, delta, Core.BORDER_DEFAULT);
		Core.convertScaleAbs(grad_x, abs_grad_x);

		Imgproc.Sobel(src_gray, grad_y, ddepth, 0, 1, 3, scale, delta, Core.BORDER_DEFAULT);
		Core.convertScaleAbs(grad_y, abs_grad_y);

		// Combine the two gradients
		Mat grad = new Mat();
		Core.addWeighted(abs_grad_x, 0.5, abs_grad_y, 0.5, 0, grad);

		return grad;

	}

	public static Mat computeDiffFrame(Mat currentAdjustedFrame, Mat prevAdjustedFrame) {
		Mat result = new Mat();
		Core.absdiff(currentAdjustedFrame, prevAdjustedFrame, result);
		// Imgproc.dilate(result, result, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(3, 3)));
		Imgproc.adaptiveThreshold(result, result, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 9, 7);
		return result;
	}

	public static void detection_contours(Mat frame, Mat diffFrame) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(diffFrame, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);

		double maxArea = 100;

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
				Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(0, 0, 255), 2);
				Imgproc.drawContours(frame, contours, i, new Scalar(0, 255, 0));

			}
		}
	}

	public static Mat adjust(Mat frame) {
		Mat result = frame.clone();
		Imgproc.cvtColor(frame, result, Imgproc.COLOR_BGR2GRAY);
		// frame = gradient(frame);
		Imgproc.GaussianBlur(result, result, new Size(3, 3), 0);
		// Imgproc.Canny(result, result, 150d, 150d * 2, 3, true);
		return result;
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

}