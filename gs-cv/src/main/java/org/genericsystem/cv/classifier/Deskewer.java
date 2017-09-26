package org.genericsystem.cv.classifier;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class Deskewer {

	private static final double closedImgSize = 5;

	public static Img deskew(Path imgPath) {
		Img img = new Img(imgPath.toString());
		Img deskewed = _deskew(img);
		img.close();
		return deskewed;
	}

	public static Img getRotatedRectanglesDrawn(final Img img, Scalar scalar, int thickness) {
		Img imgCopy = new Img(img.getSrc(), true);
		Img closed = getClosedImg(imgCopy);
		List<RotatedRect> rectangles = getRotatedRects(closed.getSrc());
		rectangles.forEach(rect -> drawSingleRotatedRectangle(imgCopy.getSrc(), rect, scalar, thickness));
		closed.close();
		return imgCopy;
	}

	private static void drawSingleRotatedRectangle(Mat mat, RotatedRect rect, Scalar scalar, int thickness) {
		Point points[] = new Point[4];
		rect.points(points);
		for (int i = 0; i < 4; ++i) {
			Imgproc.line(mat, points[i], points[(i + 1) % 4], scalar, thickness);
		}
	}

	private static Img _deskew(Img img) {
		Img closed = getClosedImg(img);
		final double angle = contoursDetection(closed.getSrc());
		System.out.println("angle: " + angle);
		Point center = new Point(img.width() / 2, img.height() / 2);
		// Rotation matrix
		Mat rotationMatrix = Imgproc.getRotationMatrix2D(center, angle, 1);

		// Get the bounding rectangle
		Rect bbox = new RotatedRect(center, img.size(), angle).boundingRect();
		// Adjust the transformation matrix
		double[] array = rotationMatrix.get(0, 2);
		array[0] += bbox.width / 2 - center.x;
		rotationMatrix.put(0, 2, array);
		array = rotationMatrix.get(1, 2);
		array[0] += bbox.height / 2 - center.y;
		rotationMatrix.put(1, 2, array);

		// Rotated Mat and empty Mat to apply the mask
		Mat rotated = new Mat(bbox.size(), CvType.CV_8UC3, Scalar.all(255));
		Mat rotatedMasked = new Mat();
		// New mask
		Mat mask = new Mat(img.size(), CvType.CV_8UC1, new Scalar(255));
		Mat warpedMask = new Mat();
		// Compute the rotation for the mask and the image
		Imgproc.warpAffine(mask, warpedMask, rotationMatrix, bbox.size());
		Imgproc.warpAffine(img.getSrc(), rotatedMasked, rotationMatrix, bbox.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
		// Apply the mask to the rotated Mat
		rotatedMasked.copyTo(rotated, warpedMask);
		// Release the matrices before return
		rotatedMasked.release();
		mask.release();
		warpedMask.release();
		rotationMatrix.release();
		closed.close();
		return new Img(rotated, false);
	}

	private static Img getClosedImg(Img img) {
		return img.adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(closedImgSize, closedImgSize));
	}

	private static double contoursDetection(Mat dilated) {
		List<RotatedRect> rotatedRects = getRotatedRects(dilated);
		double mean = 0;
		for (RotatedRect rotatedRect : rotatedRects) {
			if (rotatedRect.angle < -45.) {
				rotatedRect.angle += 90.0;
				double tmp = rotatedRect.size.width;
				rotatedRect.size.width = rotatedRect.size.height;
				rotatedRect.size.height = tmp;
			}
			mean += rotatedRect.angle;
		}
		final double average = mean / rotatedRects.size();
		List<RotatedRect> goodRects = rotatedRects.stream().filter(rotatedRect -> Math.abs(rotatedRect.angle - average) < 5).collect(Collectors.toList());
		double goodRectsMean = 0;
		for (RotatedRect rotatedRect : goodRects)
			goodRectsMean += rotatedRect.angle;
		return goodRectsMean / goodRects.size();
	}

	private static List<RotatedRect> getRotatedRects(Mat dilated) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(dilated, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		List<RotatedRect> rotatedRects = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(contour -> Imgproc.minAreaRect(new MatOfPoint2f(contour.toArray()))).collect(Collectors.toList());
		return rotatedRects;
	}
}
