package org.genericsystem.cv;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import javax.swing.ImageIcon;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.FeatureDetector;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

public class Img {
	private final Mat src = new Mat();

	public Mat getSrc() {
		return src;
	}

	public Img(Mat src) {
		src.copyTo(this.src);
	}

	public Img(Img model, Zone zone) {
		this(new Mat(model.getSrc(), zone.getRect()));
	}

	public Img sobel(int ddepth, int dx, int dy, int ksize, double scale, double delta, int borderType) {
		Mat result = new Mat();
		Imgproc.Sobel(src, result, ddepth, dx, dy, ksize, scale, delta, borderType);
		return new Img(result);
	}

	public Img adaptiveThresHold(double maxValue, int adaptiveMethod, int thresholdType, int blockSize, double C) {
		Mat result = new Mat();
		Imgproc.adaptiveThreshold(src, result, maxValue, adaptiveMethod, thresholdType, blockSize, C);
		return new Img(result);
	}

	public Img thresHold(double thresh, double maxval, int type) {
		Mat result = new Mat();
		Imgproc.threshold(src, result, thresh, maxval, type);
		return new Img(result);
	}

	public Img morphologyEx(int morphOp, StructuringElement structuringElement) {
		Mat result = new Mat();
		Imgproc.morphologyEx(src, result, morphOp, structuringElement.getSrc());
		return new Img(result);
	}

	public List<MatOfPoint> findContours(Img[] hierarchy, int mode, int method) {
		Mat mat = new Mat();
		List<MatOfPoint> result = new ArrayList<>();
		Imgproc.findContours(src, result, mat, mode, method);
		hierarchy[0] = new Img(mat);
		return result;
	}

	public List<MatOfPoint> findContours(Img[] hierarchy, int mode, int method, Point point) {
		Mat mat = new Mat();
		List<MatOfPoint> result = new ArrayList<>();
		Imgproc.findContours(src, result, mat, mode, method, point);
		hierarchy[0] = new Img(mat);
		return result;
	}

	public Img dilate(Mat kernel) {
		Mat result = new Mat();
		Imgproc.dilate(src, result, kernel);
		return new Img(result);
	}

	public Img canny(double threshold1, double threshold2) {
		Mat result = new Mat();
		Imgproc.Canny(src, result, threshold1, threshold2);
		return new Img(result);
	}

	public Img canny(double threshold1, double threshold2, int apertureSize, boolean L2gradient) {
		Mat result = new Mat();
		Imgproc.Canny(src, result, threshold1, threshold2, apertureSize, L2gradient);
		return new Img(result);
	}

	public void drawContours(List<MatOfPoint> contours, int contourIdx, Scalar color, int thickness) {
		Imgproc.drawContours(src, contours, contourIdx, color, thickness);
	}

	public Img gaussianBlur(Size ksize, double sigmaX, double sigmaY) {
		Mat result = new Mat();
		Imgproc.GaussianBlur(src, result, ksize, sigmaX, sigmaY);
		return new Img(result);
	}

	public Img medianBlur(int ksize) {
		Mat result = new Mat();
		Imgproc.medianBlur(src, result, ksize);
		return new Img(result);
	}

	public Img gray() {
		Mat result = new Mat();
		Imgproc.cvtColor(src, result, Imgproc.COLOR_BGR2GRAY);
		return new Img(result);
	}

	private static double angle(Point p1, Point p2, Point p0) {
		double dx1 = p1.x - p0.x;
		double dy1 = p1.y - p0.y;
		double dx2 = p2.x - p0.x;
		double dy2 = p2.y - p0.y;
		return (dx1 * dx2 + dy1 * dy2) / Math.sqrt((dx1 * dx1 + dy1 * dy1) * (dx2 * dx2 + dy2 * dy2) + 1e-10);
	}

	public Img cropAndDeskew() {
		Img blurred = medianBlur(9);
		Img gray = blurred.gray();
		Img gray_;

		List<MatOfPoint> contours = new ArrayList<>();

		double maxArea = 0;
		int maxId = -1;
		MatOfPoint2f maxContour = null;

		gray_ = gray.canny(10, 20, 3, true);
		gray_ = gray_.dilate(Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(12, 12)));

		contours = gray_.findContours(new Img[1], Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);

		for (MatOfPoint contour : contours) {
			MatOfPoint2f temp = new MatOfPoint2f(contour.toArray());
			double area = Imgproc.contourArea(contour);
			MatOfPoint2f approxCurve = new MatOfPoint2f();
			Imgproc.approxPolyDP(temp, approxCurve, Imgproc.arcLength(temp, true) * 0.02, true);

			if (approxCurve.total() == 4 && area >= maxArea) {
				double maxCosine = 0;

				List<Point> curves = approxCurve.toList();
				for (int j = 2; j < 5; j++) {
					double cosine = Math.abs(angle(curves.get(j % 4), curves.get(j - 2), curves.get(j - 1)));
					maxCosine = Math.max(maxCosine, cosine);
				}

				if (maxCosine < 0.3) {
					maxArea = area;
					maxId = contours.indexOf(contour);
					maxContour = approxCurve;
				}
			}
		}
		Img result = new Img(src);
		if (maxId >= 0)
			result = transform(maxContour);
		// TODO: Warning if no contour found.
		return result;
	}

	public Img transform(MatOfPoint2f contour2f) {
		Mat target = new Mat();
		List<Point> list = Arrays.asList(contour2f.toArray());
		double width = Math.max(Math.sqrt(Math.pow(list.get(0).x - list.get(1).x, 2) + Math.pow(list.get(0).y - list.get(1).y, 2)), Math.sqrt(Math.pow(list.get(2).x - list.get(3).x, 2) + Math.pow(list.get(2).y - list.get(3).y, 2)));
		double height = Math.max(Math.sqrt(Math.pow(list.get(1).x - list.get(2).x, 2) + Math.pow(list.get(1).y - list.get(2).y, 2)), Math.sqrt(Math.pow(list.get(3).x - list.get(0).x, 2) + Math.pow(list.get(3).y - list.get(0).y, 2)));
		boolean toReverse = width < height;
		if (toReverse) {
			System.out.println("inversion width height");
			double tmp = width;
			width = height;
			height = tmp;
		}
		List<Point> targets = new LinkedList<>(Arrays.asList(new Point(width, 0), new Point(0, 0), new Point(0, height), new Point(width, height)));
		if (toReverse) {
			Point first = targets.get(0);
			targets.remove(0);
			targets.add(first);
		}
		Imgproc.warpPerspective(src, target, Imgproc.getPerspectiveTransform(contour2f, Converters.vector_Point2f_to_Mat(targets)), new Size(width, height), Imgproc.INTER_CUBIC);
		return new Img(target);
	}

	public Size size() {
		return src.size();
	}

	public int height() {
		return src.height();
	}

	public int width() {
		return src.width();
	}

	public double[] get(int row, int col) {
		return src.get(row, col);
	}

	public Img cvtColor(int code) {
		Mat result = new Mat();
		Imgproc.cvtColor(src, result, code);
		return new Img(result);
	}

	public ImageIcon getImageIcon() {
		return new ImageIcon(Tools.mat2bufferedImage(src));
	}

	public void rectangle(Rect rect, Scalar color, int thickNess) {
		Imgproc.rectangle(src, rect.br(), rect.tl(), color, thickNess);
	}

	public Node getImageView() {
		Mat conv = new Mat();
		src.convertTo(conv, CvType.CV_8UC1);
		Mat target = new Mat();
		Imgproc.resize(conv, target, new Size(AbstractApp.displayWidth, Math.floor((AbstractApp.displayWidth / conv.width()) * conv.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(AbstractApp.displayWidth);
		return imageView;
	}

	public int channels() {
		return src.channels();
	}

	public Img range(Scalar scalar, Scalar scalar2, boolean hsv) {
		Img ranged = this;
		if (hsv)
			ranged = ranged.cvtColor(Imgproc.COLOR_BGR2HSV);
		Mat result = new Mat(ranged.size(), ranged.type(), new Scalar(0, 0, 0));
		Mat mask = new Mat();
		Core.inRange(ranged.getSrc(), scalar, scalar2, mask);
		ranged.getSrc().copyTo(result, mask);
		Img resultImg = new Img(result);
		if (hsv)
			resultImg = resultImg.cvtColor(Imgproc.COLOR_HSV2BGR);
		return resultImg;
	}

	public int type() {
		return src.type();
	}

	public Img gaussianBlur(Size size) {
		Mat result = new Mat();
		Imgproc.GaussianBlur(src, result, size, 0);
		return new Img(result);
	}

	public Img multiply(Scalar scalar) {
		Mat result = new Mat();
		Core.multiply(src, scalar, result);
		return new Img(result);
	}

	public Img sobel() {
		Img gray = cvtColor(Imgproc.COLOR_BGR2GRAY);
		Img sobel = gray.sobel(CvType.CV_8UC1, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT);
		Img threshold = sobel.thresHold(0, 255, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		return threshold.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 3)));
	}

	public Img grad() {
		Img gray = cvtColor(Imgproc.COLOR_BGR2GRAY);
		Img grad = gray.morphologyEx(Imgproc.MORPH_GRADIENT, new StructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));
		Img threshold = grad.thresHold(0.0, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		return threshold.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 3)));
	}

	public Img mser() {
		Img gray = cvtColor(Imgproc.COLOR_BGR2GRAY);
		MatOfKeyPoint keypoint = new MatOfKeyPoint();
		FeatureDetector detector = FeatureDetector.create(FeatureDetector.MSER);
		detector.detect(gray.getSrc(), keypoint);
		List<KeyPoint> listpoint = keypoint.toList();
		Mat result = Mat.zeros(gray.size(), CvType.CV_8UC1);
		for (int ind = 0; ind < listpoint.size(); ind++) {
			KeyPoint kpoint = listpoint.get(ind);
			int rectanx1 = (int) (kpoint.pt.x - 0.5 * kpoint.size);
			int rectany1 = (int) (kpoint.pt.y - 0.5 * kpoint.size);
			int width = (int) (kpoint.size);
			int height = (int) (kpoint.size);
			if (rectanx1 <= 0)
				rectanx1 = 1;
			if (rectany1 <= 0)
				rectany1 = 1;
			if ((rectanx1 + width) > gray.width())
				width = gray.width() - rectanx1;
			if ((rectany1 + height) > gray.height())
				height = gray.height() - rectany1;
			Rect rectant = new Rect(rectanx1, rectany1, width, height);
			Mat roi = new Mat(result, rectant);
			roi.setTo(new Scalar(255));
		}
		return new Img(result).morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 3)));
	}

	public Img otsu() {
		return cvtColor(Imgproc.COLOR_BGR2GRAY).thresHold(0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
	}

	public Img otsuInv() {
		return cvtColor(Imgproc.COLOR_BGR2GRAY).thresHold(0, 255, Imgproc.THRESH_BINARY_INV + Imgproc.THRESH_OTSU);
	}

	public Img dilateBlacks(int valueThreshold, int saturatioThreshold, int blueThreshold, Size dilatation) {
		return range(new Scalar(0, 0, 0), new Scalar(255, saturatioThreshold, valueThreshold), true).range(new Scalar(0, 0, 0), new Scalar(blueThreshold, 255, 255), false).gray()
				.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, dilatation));
	}

	public Img equalizeHisto(Mat mat) {
		Mat result = new Mat();
		Imgproc.cvtColor(mat, result, Imgproc.COLOR_BGR2YCrCb);
		List<Mat> channels = new ArrayList<Mat>();
		Core.split(result, channels);
		Imgproc.equalizeHist(channels.get(0), channels.get(0));
		Imgproc.equalizeHist(channels.get(1), channels.get(1));
		Imgproc.equalizeHist(channels.get(2), channels.get(2));
		Core.merge(channels, result);
		Imgproc.cvtColor(result, result, Imgproc.COLOR_YCrCb2BGR);
		return new Img(result);
	}

	public Img resize(Size size) {
		Mat result = new Mat();
		Imgproc.resize(src, result, size);
		return new Img(result);
	}

	public Img resize(double coeff) {
		Mat result = new Mat();
		Imgproc.resize(src, result, new Size(src.width() * coeff, src.height() * coeff));
		return new Img(result);
	}

}
