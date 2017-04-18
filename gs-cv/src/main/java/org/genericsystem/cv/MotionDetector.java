package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
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

	public static void main(String[] args) {
		JFrame jframe = new JFrame("Motion Detector");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);
		Mat frame = new Mat();
		VideoCapture camera = new VideoCapture(0);
		camera.read(frame);
		jframe.setSize(frame.width(), frame.height());
		jframe.setVisible(true);
		Mat prevAdjustedFrame = adjust(frame);
		Mat average = prevAdjustedFrame;
		double n = 50;
		while (camera.read(frame)) {
			Mat currentAdjustedFrame = adjust(frame);
			Core.addWeighted(average, (n - 1) / n, currentAdjustedFrame, 10d / n, 0, average);
			Mat diffFrame = computeDiffFrame(currentAdjustedFrame, average);
			prevAdjustedFrame = currentAdjustedFrame;
			for (Rect rect : detection_contours(frame, diffFrame))
				Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(0, 0, 255), 1);

			ImageIcon image = new ImageIcon(mat2bufferedImage(frame));
			vidpanel.setIcon(image);
			vidpanel.repaint();
		}
	}

	public static Mat computeDiffFrame(Mat currentAdjustedFrame, Mat prevAdjustedFrame) {
		Mat result = new Mat();
		Core.absdiff(currentAdjustedFrame, prevAdjustedFrame, result);
		Imgproc.adaptiveThreshold(result, result, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 9, 3);
		return result;
	}

	public static List<Rect> detection_contours(Mat frame, Mat diffFrame) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(diffFrame, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);

		double maxArea = 100;
		List<Rect> rectangles = new ArrayList<>();
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
				Point[] result = new Point[4];
				Imgproc.minAreaRect(contour2F).points(result);
				Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 2);
				rectangles.add(Imgproc.boundingRect(contour));
				Imgproc.drawContours(frame, contours, i, new Scalar(0, 255, 0));
			}
		}
		return rectangles;

	}

	public static Mat adjust(Mat frame) {
		Mat result = frame.clone();
		Imgproc.cvtColor(frame, result, Imgproc.COLOR_BGR2GRAY);
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