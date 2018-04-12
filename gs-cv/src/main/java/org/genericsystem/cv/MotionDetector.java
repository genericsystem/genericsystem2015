package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.genericsystem.cv.utils.NativeLibraryLoader;
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
		NativeLibraryLoader.load();
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
		while (camera.read(frame)) {
			Mat copy = frame.clone();
			Imgproc.cvtColor(frame, frame, Imgproc.COLOR_BGR2GRAY);
			Imgproc.GaussianBlur(frame, frame, new Size(13, 13), 0);
			Imgproc.adaptiveThreshold(frame, frame, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 51, 2);
			Imgproc.morphologyEx(frame, frame, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(9, 9)));
			Imgproc.morphologyEx(frame, frame, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(5, 5)));

			List<MatOfPoint> contours = new ArrayList<>();
			Imgproc.findContours(frame, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
			for (MatOfPoint contour : contours)
				Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(contour)), 0, new Scalar(255, 0, 0), -1);

			Imgproc.morphologyEx(frame, frame, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(4, 4)));
			Mat houghLines = new Mat();
			Imgproc.HoughLinesP(frame, houghLines, 1, Math.PI / 180, 10, 100, 10);
			Mat result = copy;
			Lines horizontals = new Lines(new Lines(houghLines).getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) < Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
			horizontals.draw(result, new Scalar(0, 0, 255), 1);

			Lines verticals = new Lines(new Lines(houghLines).getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) > Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
			verticals.draw(result, new Scalar(0, 255, 0), 1);

			ImageIcon image = new ImageIcon(mat2bufferedImage(result));
			vidpanel.setIcon(image);
			vidpanel.repaint();
		}
	}

	public static List<Rect> detection_contours(Mat frame, Mat diffFrame) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(diffFrame, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double maxArea = 10;
		List<Rect> rectangles = new ArrayList<>();
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > maxArea) {
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				Point[] result = new Point[4];
				Imgproc.minAreaRect(contour2F).points(result);
				Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 1);
				rectangles.add(Imgproc.boundingRect(contour));
				Imgproc.drawContours(frame, contours, i, new Scalar(0, 255, 0));
			}
		}
		return rectangles;

	}

	public static Mat adjust(Mat frame) {
		Mat result = new Mat();
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