package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.Core.MinMaxLocResult;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfDouble;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.MSER;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
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
		// HOGDescriptor hog = new HOGDescriptor();
		while (camera.read(frame)) {
			// Mat diffFrame = new Img(frame, false).bilateralFilter().bgr2Gray().getSrc();
			// Core.absdiff(diffFrame, new Scalar(0), diffFrame);
			// Imgproc.adaptiveThreshold(diffFrame, diffFrame, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);

			// Core.inRange(diffFrame, new Scalar(127), new Scalar(255), diffFrame);

			// Mat kernel = Mat.ones(10, 5, CvType.CV_8U);
			// Imgproc.dilate(diffFrame, diffFrame, kernel);

			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_DILATE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 2)));
			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_DILATE, Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(5, 10)));
			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(10, 10)));
			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));
			// Imgproc.resize(diffFrame, diffFrame, new Size(diffFrame.width() / 4, diffFrame.height() / 4));
			// Imgproc.resize(diffFrame, diffFrame, new Size(diffFrame.width() * 4, diffFrame.height() * 4));
			// Imgproc.GaussianBlur(diffFrame, diffFrame, new Size(3, 3), 0);
			// Imgproc.threshold(diffFrame, diffFrame, 10, 255, Imgproc.THRESH_BINARY);
			// Mat mask = new Mat();
			// Imgproc.morphologyEx(diffFrame, mask, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(10, 10)));
			// Imgproc.morphologyEx(mask, mask, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(10, 10)));
			// Imgproc.morphologyEx(mask, mask, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(15, 15)));
			// Imgproc.morphologyEx(mask, mask, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(15, 15)));
			// Imgproc.morphologyEx(diffFrame, mask, Imgproc.MORPH_DILATE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(5, 5)));
			// Imgproc.morphologyEx(diffFrame, mask, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(15, 15)));
			// Imgproc.morphologyEx(mask, mask, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(7, 7)));
			// Imgproc.morphologyEx(mask, mask, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));

			// Mat lines = new Mat();
			// Imgproc.HoughLinesP(mask, lines, 1, Math.PI / 180, 10, 20, 4);
			// Mat result = new Mat(mask.size(), mask.type(), new Scalar(0));
			// new Lines(lines).draw(result, new Scalar(255), 2);

			// Mat result = new Mat(diffFrame.size(), diffFrame.type(), new Scalar(0));
			// diffFrame.copyTo(result, mask);
			// Imgproc.GaussianBlur(result, result, new Size(3, 3), 0);
			// Imgproc.threshold(result, result, 1, 255, Imgproc.THRESH_BINARY);
			// Imgproc.morphologyEx(result, mask, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(15, 15)));
			// Imgproc.morphologyEx(mask, mask, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(15, 15)));

			// Mat result2 = new Mat(result.size(), diffFrame.type(), new Scalar(0));
			// result.copyTo(result2, mask);

			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));

			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_DILATE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));

			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 2)));

			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(3, 3)));
			// Imgproc.morphologyEx(diffFrame, diffFrame, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));
			// Imgproc.threshold(diffFrame, diffFrame, 127, 255, Imgproc.THRESH_BINARY);
			// detection_contours(frame, diffFrame);

			// MatOfKeyPoint keypoint = new MatOfKeyPoint();
			MSER detector = MSER.create(3, 10, 2000, 0.25, 0.1, 100, 1.01, 0.03, 5);
			Img gray = new Img(frame, false).bgr2Gray();

			// detector.detect(gray.getSrc(), keypoint);
			ArrayList<MatOfPoint> regions = new ArrayList<>();
			MatOfRect mor = new MatOfRect();
			detector.detectRegions(gray.getSrc(), regions, mor);
			// System.out.println(mor);
			Mat mserMask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(0));
			for (MatOfPoint mop : regions) {
				for (Point p : mop.toList())
					mserMask.put((int) p.y, (int) p.x, 255);
			}
			// for (Rect rect : mor.toList()) {
			// Imgproc.rectangle(mserMask, rect.tl(), rect.br(), new Scalar(255), -1);
			// // mserMask.put((int) kpoint.pt.x, (int) kpoint.pt.y, 255);
			// }
			Mat edges = new Mat();
			Imgproc.Canny(gray.getSrc(), edges, 20, 100);
			Mat edge_mser_intersection = new Mat();
			Core.bitwise_and(edges, mserMask, edge_mser_intersection);

			Mat gradientGrown = growEdges(gray.getSrc(), edge_mser_intersection);
			Mat edgeEnhancedMser = new Mat();
			Mat notGradientGrown = new Mat();
			Core.bitwise_not(gradientGrown, notGradientGrown);
			Core.bitwise_and(notGradientGrown, mserMask, edgeEnhancedMser);

			Mat labels = new Mat();
			Mat stats = new Mat();
			Mat centroid = new Mat();
			int labelsIds = Imgproc.connectedComponentsWithStats(edgeEnhancedMser, labels, stats, centroid, 4, CvType.CV_32S);
			Mat result2 = new Mat(labels.size(), CvType.CV_8UC1, new Scalar(0));
			for (int labelId = 0; labelId < labelsIds; labelId++) {
				double area = stats.get(labelId, Imgproc.CC_STAT_AREA)[0];
				if (area < 3 || area > 600)
					continue;

				Mat labelMask = new Mat();
				Core.inRange(labels, new Scalar(labelId), new Scalar(labelId), labelMask);
				Core.bitwise_or(result2, labelMask, result2);
			}

			Imgproc.distanceTransform(result2, result2, Imgproc.DIST_L2, 3);
			result2.convertTo(result2, CvType.CV_32SC1);
			// Core.multiply(result2, new Scalar(50), result2);
			Mat strokeWidth = computeStrokeWidth(result2);
			// Core.multiply(stokeWidth, new Scalar(50), stokeWidth);
			Mat filtered_stroke_width = new Mat(strokeWidth.size(), CvType.CV_8UC1, new Scalar(0));

			Mat strokeWithCV8U = new Mat();
			strokeWidth.convertTo(strokeWithCV8U, CvType.CV_8UC1);
			labelsIds = Imgproc.connectedComponentsWithStats(strokeWithCV8U, labels, stats, centroid, 4, CvType.CV_32S);
			for (int labelId = 0; labelId < labelsIds; labelId++) {
				Mat labelMask = new Mat();
				Core.inRange(labels, new Scalar(labelId), new Scalar(labelId), labelMask);
				Mat temp = new Mat(strokeWithCV8U.size(), strokeWithCV8U.type(), new Scalar(0));
				strokeWithCV8U.copyTo(temp, labelMask);
				int area = Core.countNonZero(temp);
				MatOfDouble meanD = new MatOfDouble();
				MatOfDouble stdDev = new MatOfDouble();
				Core.meanStdDev(strokeWithCV8U, meanD, stdDev, labelMask);

				if (area != 0) {
					/* Filter out those which are out of the prespecified ratio */
					if ((stdDev.get(0, 0)[0] / meanD.get(0, 0)[0]) > 0.5)
						continue;

					/* Collect the filtered stroke width */
					Core.bitwise_or(filtered_stroke_width, labelMask, filtered_stroke_width);
				}
			}

			Mat bounding_region = new Mat();
			Imgproc.morphologyEx(filtered_stroke_width, bounding_region, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(25, 25)));
			Imgproc.morphologyEx(bounding_region, bounding_region, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(7, 7)));
			Mat result3 = new Mat();
			frame.copyTo(result3, bounding_region);

			ImageIcon image = new ImageIcon(mat2bufferedImage(filtered_stroke_width));
			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

	private static int booleansToInt(boolean[] arr) {
		int n = 0;
		for (boolean b : arr)
			n = (n << 1) | (b ? 1 : 0);
		// int i = 0;
		// for (boolean bool : arr) {
		// assert ((n & ((int) Math.pow(2, 7 - i++))) != 0) == bool;
		// }
		return n;
	}

	private static int getNeighborsLessThan(Mat mat, int y, int x) {
		boolean[] neighbors = new boolean[8];
		neighbors[0] = mat.get(y, x - 1)[0] == 0 ? false : mat.get(y, x - 1)[0] < mat.get(y, x)[0];
		neighbors[1] = mat.get(y - 1, x - 1)[0] == 0 ? false : mat.get(y - 1, x - 1)[0] < mat.get(y, x)[0];
		neighbors[2] = mat.get(y - 1, x)[0] == 0 ? false : mat.get(y - 1, x)[0] < mat.get(y, x)[0];
		neighbors[3] = mat.get(y - 1, x + 1)[0] == 0 ? false : mat.get(y - 1, x + 1)[0] < mat.get(y, x)[0];
		neighbors[4] = mat.get(y, x + 1)[0] == 0 ? false : mat.get(y, x + 1)[0] < mat.get(y, x)[0];
		neighbors[5] = mat.get(y + 1, x + 1)[0] == 0 ? false : mat.get(y + 1, x + 1)[0] < mat.get(y, x)[0];
		neighbors[6] = mat.get(y + 1, x)[0] == 0 ? false : mat.get(y + 1, x)[0] < mat.get(y, x)[0];
		neighbors[7] = mat.get(y + 1, x - 1)[0] == 0 ? false : mat.get(y + 1, x - 1)[0] < mat.get(y, x)[0];
		return booleansToInt(neighbors);
	}

	private static Mat computeStrokeWidth(Mat dist) {
		/* Pad the distance transformed matrix to avoid boundary checking */
		Mat padded = new Mat(dist.rows() + 1, dist.cols() + 1, dist.type(), new Scalar(0));
		dist.copyTo(new Mat(padded, new Rect(1, 1, dist.cols(), dist.rows())));

		Mat lookup = new Mat(padded.size(), CvType.CV_8UC1, new Scalar(0));
		for (int y = 1; y < padded.rows() - 1; y++) {
			for (int x = 1; x < padded.cols() - 1; x++) {
				/* Extract all the neighbors whose value < curr_ptr[x], encoded in 8-bit uchar */
				if (padded.get(y, x)[0] != 0)
					lookup.put(y, x, (double) getNeighborsLessThan(padded, y, x));
			}
		}

		/* Get max stroke from the distance transformed */
		MinMaxLocResult minMaxLocResult = Core.minMaxLoc(padded);
		int maxStroke = (int) Math.round(minMaxLocResult.maxVal);
		for (double stroke = maxStroke; stroke > 0; stroke--) {
			Mat stroke_indices_mat = new Mat();
			Mat mask = new Mat();
			Core.inRange(padded, new Scalar(stroke - 0.1), new Scalar(stroke + 0.1), mask);

			Mat masked = new Mat();
			padded.copyTo(masked, mask);
			masked.convertTo(masked, CvType.CV_8UC1);
			Core.findNonZero(masked, stroke_indices_mat);
			List<Point> stroke_indices = new ArrayList<>();
			if (stroke_indices_mat.cols() > 0)
				Converters.Mat_to_vector_Point(stroke_indices_mat, stroke_indices);

			List<Point> neighbors = new ArrayList<>();
			for (Point stroke_index : stroke_indices) {
				List<Point> temp = convertToCoords((int) stroke_index.x, (int) stroke_index.y, (int) lookup.get((int) stroke_index.y, (int) stroke_index.x)[0]);
				neighbors.addAll(temp);
			}

			while (!neighbors.isEmpty()) {
				for (Point neighbor : neighbors)
					padded.put((int) neighbor.y, (int) neighbor.x, stroke);
				neighbors.clear();
				List<Point> temp = new ArrayList<>(neighbors);
				neighbors.clear();
				/* Recursively gets neighbors of the current neighbors */
				for (Point neighbor : temp) {
					List<Point> temp2 = convertToCoords((int) neighbor.x, (int) neighbor.y, (int) lookup.get((int) neighbor.y, (int) neighbor.x)[0]);
					neighbors.addAll(temp2);
				}
			}

		}

		return new Mat(padded, new Rect(1, 1, dist.cols(), dist.rows()));
	}

	private static List<Point> convertToCoords(int x, int y, int neighbors) {
		List<Point> coords = new ArrayList<>();
		if (((neighbors & ((int) Math.pow(2, 7))) != 0))
			coords.add(new Point(x - 1, y));
		if (((neighbors & ((int) Math.pow(2, 6))) != 0))
			coords.add(new Point(x - 1, y - 1));
		if (((neighbors & ((int) Math.pow(2, 5))) != 0))
			coords.add(new Point(x, y - 1));
		if (((neighbors & ((int) Math.pow(2, 4))) != 0))
			coords.add(new Point(x + 1, y - 1));
		if (((neighbors & ((int) Math.pow(2, 3))) != 0))
			coords.add(new Point(x + 1, y));
		if (((neighbors & ((int) Math.pow(2, 2))) != 0))
			coords.add(new Point(x + 1, y + 1));
		if (((neighbors & ((int) Math.pow(2, 1))) != 0))
			coords.add(new Point(x, y + 1));
		if (((neighbors & ((int) Math.pow(2, 0))) != 0))
			coords.add(new Point(x - 1, y + 1));
		return coords;
	}

	public static int toBin(double angle, int neighbors) {
		float divisor = 180.0f / neighbors;
		return (int) (((Math.floor(angle / divisor) - 1) / 2) + 1) % neighbors + 1;
	}

	public static Mat growEdges(Mat image, Mat edges) {

		Mat grad_x = new Mat(), grad_y = new Mat();
		Imgproc.Sobel(image, grad_x, CvType.CV_64FC1, 1, 0);
		Imgproc.Sobel(image, grad_y, CvType.CV_64FC1, 0, 1);
		Core.subtract(Mat.zeros(image.size(), CvType.CV_64FC1), grad_x, grad_x);
		Mat grad_mag = new Mat(), grad_dir = new Mat();
		Core.cartToPolar(grad_x, grad_y, grad_mag, grad_dir, true);

		/*
		 * Convert the angle into predefined 3x3 neighbor locations | 2 | 3 | 4 | | 1 | 0 | 5 | | 8 | 7 | 6 |
		 */
		for (int y = 0; y < grad_dir.rows(); y++)
			for (int x = 0; x < grad_dir.cols(); x++)
				grad_dir.put(y, x, toBin((grad_dir.get(y, x))[0], 8));
		grad_dir.convertTo(grad_dir, CvType.CV_8UC1);

		/* Perform region growing based on the gradient direction */
		Mat result = new Mat();
		edges.copyTo(result);
		for (int y = 1; y < edges.rows() - 1; y++) {
			for (int x = 1; x < edges.cols() - 1; x++) {
				/* Only consider the contours */
				if (edges.get(y, x)[0] != 0) {

					/* .. there should be a better way .... */
					switch ((int) grad_dir.get(y, x)[0]) {
					case 1:
						result.put(y, x - 1, 255);
						break;
					case 2:
						result.put(y - 1, x - 1, 255);
						break;
					case 3:
						result.put(y - 1, x, 255);
						break;
					case 4:
						result.put(y - 1, x + 1, 255);
						break;
					case 5:
						result.put(y, x, 255);
						break;
					case 6:
						result.put(y + 1, x + 1, 255);
						break;
					case 7:
						result.put(y + 1, x, 255);
						break;
					case 8:
						result.put(y + 1, x - 1, 255);
						break;
					default:
						System.out.println("Error : " + (int) grad_dir.get(y, x)[0]);
						break;
					}
				}
			}

		}

		return result;
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
				// Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 1);
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