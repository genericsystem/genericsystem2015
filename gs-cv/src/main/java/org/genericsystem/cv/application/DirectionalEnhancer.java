package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Lines;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class DirectionalEnhancer {

	public static Lines getLines(Mat frame) {
		Mat mat = prepare(frame);
		Mat houghLines = new Mat();
		Imgproc.HoughLinesP(mat, houghLines, 1, Math.PI / 180, 10, 100, 10);
		mat.release();
		Lines lines = new Lines(houghLines);
		houghLines.release();
		return lines;
	}

	public static void drawFilteredLines(Mat binarized, Lines houghLines) {

		Lines horizontalLines = new Lines(houghLines.getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) < Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
		horizontalLines.draw(binarized, new Scalar(255), 1);

		Lines verticalLines = new Lines(houghLines.getLines().stream().filter(l -> Math.abs(l.y2 - l.y1) > Math.abs(l.x2 - l.x1)).collect(Collectors.toList()));
		verticalLines.draw(binarized, new Scalar(255), 1);
	}

	public static Mat prepare(Mat frame) {
		Mat mat = new Mat();
		Imgproc.cvtColor(frame, mat, Imgproc.COLOR_BGR2GRAY);
		Imgproc.GaussianBlur(mat, mat, new Size(13, 13), 0);
		Imgproc.adaptiveThreshold(mat, mat, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 51, 2);
		Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(9, 9)));
		Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(5, 5)));

		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(mat, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (MatOfPoint contour : contours)
			Imgproc.drawContours(mat, Arrays.asList(contour), 0, new Scalar(255, 0, 0), -1);

		Imgproc.morphologyEx(mat, mat, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(4, 4)));
		return mat;
	}

}
