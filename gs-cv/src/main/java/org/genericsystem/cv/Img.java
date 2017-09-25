package org.genericsystem.cv;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import javax.swing.ImageIcon;

import org.genericsystem.layout.Layout;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfFloat;
import org.opencv.core.MatOfInt;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.FeatureDetector;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.CLAHE;
import org.opencv.imgproc.Imgproc;
import org.opencv.objdetect.HOGDescriptor;
import org.opencv.photo.Photo;
import org.opencv.utils.Converters;
import org.opencv.ximgproc.Ximgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Img implements AutoCloseable, Serializable {

	private static Logger log = LoggerFactory.getLogger(Img.class);

	private final Mat src;

	public Mat getSrc() {
		return src;
	}

	public Img(String path) {
		this(Imgcodecs.imread(path), false);
	}

	public Img(Mat src) {
		this(src, true);
	}

	public Img(Mat src, boolean clone) {
		if (clone) {
			this.src = new Mat();
			src.copyTo(this.src);
		} else
			this.src = src;
	}

	public Img(Img model, Zone zone) {
		this.src = new Mat(model.getSrc(), zone.getRect());
	}

	public Img(Img model, Layout shard) {
		this.src = new Mat(model.getSrc(), new Rect(new Point(shard.getX1() * model.width(), shard.getY1() * model.height()), new Point(shard.getX2() * model.width(), shard.getY2() * model.height())));
	}

	public Img morphologyEx(int morphOp, int morph, Size size) {
		Mat result = new Mat();
		Imgproc.morphologyEx(src, result, morphOp, Imgproc.getStructuringElement(morph, size));
		// // Imgproc.morphologyEx(src, result, morphOp, Imgproc.getStructuringElement(morph, size), new Point(-1, -1), 1, Core.BORDER_CONSTANT, new Scalar(255));
		return new Img(result, false);
	}

	public Img laplacian() {
		Mat result = new Mat();
		Imgproc.Laplacian(src, result, CvType.CV_8U);
		return new Img(result, false);
	}

	public List<MatOfPoint> findContours(Img[] hierarchy, int mode, int method) {
		Mat mat = new Mat();
		List<MatOfPoint> result = new ArrayList<>();
		Imgproc.findContours(src, result, mat, mode, method);
		hierarchy[0] = new Img(mat, false);
		return result;
	}

	public List<MatOfPoint> findContours(Img[] hierarchy, int mode, int method, Point point) {
		Mat mat = new Mat();
		List<MatOfPoint> result = new ArrayList<>();
		Imgproc.findContours(src, result, mat, mode, method, point);
		hierarchy[0] = new Img(mat, false);
		return result;
	}

	public Img dilate(Mat kernel) {
		Mat result = new Mat();
		Imgproc.dilate(src, result, kernel);
		return new Img(result, false);
	}

	public Img canny(double threshold1, double threshold2) {
		Mat result = new Mat();
		Imgproc.Canny(src, result, threshold1, threshold2);
		return new Img(result, false);
	}

	public Img canny(double threshold1, double threshold2, int apertureSize, boolean L2gradient) {
		Mat result = new Mat();
		Imgproc.Canny(src, result, threshold1, threshold2, apertureSize, L2gradient);
		return new Img(result, false);
	}

	public void drawContours(List<MatOfPoint> contours, int contourIdx, Scalar color, int thickness) {
		Imgproc.drawContours(src, contours, contourIdx, color, thickness);
	}

	public Img gaussianBlur(Size ksize, double sigmaX, double sigmaY) {
		Mat result = new Mat();
		Imgproc.GaussianBlur(src, result, ksize, sigmaX, sigmaY);
		return new Img(result, false);
	}

	public Img medianBlur(int ksize) {
		Mat result = new Mat();
		Imgproc.medianBlur(src, result, ksize);
		return new Img(result, false);
	}

	public Img bgr2Gray() {
		Mat result = new Mat();
		Imgproc.cvtColor(src, result, Imgproc.COLOR_BGR2GRAY);
		return new Img(result, false);
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
		Img gray;
		if (src.channels() == 1)
			gray = blurred;
		else
			gray = blurred.bgr2Gray();
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
		Img result;
		if (maxId >= 0)
			result = transform(maxContour);
		else
			result = new Img(src);
		// TODO: Warning if no contour found.
		blurred.close();
		gray.close();
		gray_.close();
		return result;
	}

	public Img transform(MatOfPoint2f contour2f) {
		List<Point> list = new ArrayList<>(Arrays.asList(contour2f.toArray()));

		// Put the points in counterclockwise order.
		if (isClockwise(list)) {
			Point second = list.remove(3);
			Point fourth = list.remove(1);
			list.add(1, second);
			list.add(fourth);
		}

		// Look for the top left corner of the rectangle.
		// The line used as the top of the rectangle makes an angle of 45° max
		// with an horizontal line.
		int yMinIndex = 0; // Point with min y, and min x if there are two such
		// points.
		int xMinIndex = 0; // Point with min x, and min y if there are two such
		// points.
		for (int i = 0; i < list.size(); i++) {
			double xCurr = list.get(i).x;
			double xMin = list.get(xMinIndex).x;
			double yCurr = list.get(i).y;
			double yMin = list.get(yMinIndex).y;
			if (xCurr < xMin || xCurr == xMin && yCurr < list.get(xMinIndex).y)
				xMinIndex = i;
			if (yCurr < yMin || yCurr == yMin && xCurr < list.get(yMinIndex).x)
				yMinIndex = i;
		}
		int tlIndex = yMinIndex;
		if (yMinIndex != xMinIndex) {
			double slope = (list.get(xMinIndex).y - list.get(yMinIndex).y) / (list.get(yMinIndex).x - list.get(xMinIndex).x);
			if (slope < 1)
				tlIndex = xMinIndex;
		}

		// Put the top left corner first.
		for (int i = 0; i < tlIndex; i++)
			list.add(list.remove(0));

		// Transform the image.
		double height = distance(list.get(0), list.get(1));
		double width = distance(list.get(1), list.get(2));
		Mat target = new Mat();
		List<Point> targets = new LinkedList<>(Arrays.asList(new Point(0, 0), new Point(0, height), new Point(width, height), new Point(width, 0)));
		Imgproc.warpPerspective(src, target, Imgproc.getPerspectiveTransform(Converters.vector_Point2f_to_Mat(list), Converters.vector_Point2f_to_Mat(targets)), new Size(width, height), Imgproc.INTER_CUBIC);

		Img result = new Img(target, false);
		int orientation = result.getOrientation();
		if (orientation != 0)
			result = result.rotate(orientation);
		return result;
	}

	private double distance(Point p1, Point p2) {
		return Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2));
	}

	// angle is 90, 180 or 270 degrees
	public Img rotate(int angle) {
		Mat result = new Mat();
		if (angle == 90) {
			Core.transpose(src, result);
			Core.flip(result, result, 0);
		}
		if (angle == 180)
			Core.flip(src, result, -1);
		if (angle == 270) {
			Core.transpose(src, result);
			Core.flip(result, result, 1);
		}
		return new Img(result, false);
	}

	// List of points corresponding to the ordered vertices of a convex polygon.
	private boolean isClockwise(List<Point> points) {
		Point p1 = points.get(0);
		Point p2 = points.get(1);
		Point p3 = points.get(2);
		// The points are in clockwise order iff the determinant of the vectors
		// p1p2 and p2p3 is positive. (/!\ clockwise basis)
		return (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x) >= 0;
	}

	public int getOrientation() {
		try {
			File tmpFile = File.createTempFile("orientation", ".png");
			tmpFile.deleteOnExit();
			Imgcodecs.imwrite(tmpFile.toString(), src);
			Process process = Runtime.getRuntime().exec(new String[] { "../gs-cv/orientation.sh", tmpFile.toString() });
			process.waitFor();
			BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));
			return Integer.valueOf(stdInput.readLine());
		} catch (IOException | InterruptedException e) {
			log.warn("Impossible to detect file orientation, returning 0.", e);
			return 0;
		}
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
		return new Img(result, false);
	}

	public ImageIcon getImageIcon() {
		return new ImageIcon(Tools.mat2bufferedImage(src));
	}

	public void rectangle(Rect rect, Scalar color, int thickNess) {
		Imgproc.rectangle(src, rect.br(), rect.tl(), color, thickNess);
	}

	public ImageView getImageView() {
		return getImageView(AbstractApp.displayWidth);
	}

	public ImageView getImageView(double width) {
		Mat conv = new Mat();
		src.convertTo(conv, CvType.CV_8UC1);
		Mat target = new Mat();
		Imgproc.resize(conv, target, new Size(width, Math.floor((width / conv.width()) * conv.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(width);
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
		Img resultImg = new Img(result, false);
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
		return new Img(result, false);
	}

	public Img multiply(Scalar scalar) {
		Mat result = new Mat();
		Core.multiply(src, scalar, result);
		return new Img(result, false);
	}

	public Img mser() {
		Img gray = bgr2Gray();
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
		Img img = new Img(result, false);
		Img result_ = img.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(17, 3));
		img.close();
		return result_;
	}

	public Img grad(double k1, double k2) {
		// Img gray = bgr2Gray();
		Img grad = morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(k1, k2));
		return grad;
		// return grad.thresHold(0.0, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		// return threshold.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(17, 3));
	}

	// public Img classic() {
	// Img gray = gray();
	// Img threshold = gray.thresHold(0, 255, Imgproc.THRESH_OTSU +
	// Imgproc.THRESH_BINARY);
	// return threshold.morphologyEx(Imgproc.MORPH_CLOSE, new
	// StructuringElement(Imgproc.MORPH_RECT, new Size(17, 3)));
	// }

	public Img sobel() {
		Img gray = bgr2Gray();
		Img sobel = gray.sobel(CvType.CV_8UC1, 1, 1, 5, 1, 0, Core.BORDER_DEFAULT);
		// Img threshold = sobel.thresHold(0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
		return sobel;
	}

	public Img bernsen() {
		return bernsen(31, 15);
	}

	public Img otsu() {
		return bgr2Gray().thresHold(0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
	}

	public Img otsuAfterGaussianBlur() {
		return otsuAfterGaussianBlur(new Size(5, 5));
	}

	public Img otsuInv() {
		return bgr2Gray().thresHold(0, 255, Imgproc.THRESH_BINARY_INV + Imgproc.THRESH_OTSU);
	}

	public Img equalizeHisto() {
		Mat result = new Mat();
		List<Mat> channels = new ArrayList<>();
		Core.split(src, channels);
		Imgproc.equalizeHist(channels.get(0), channels.get(0));
		Imgproc.equalizeHist(channels.get(1), channels.get(1));
		Imgproc.equalizeHist(channels.get(2), channels.get(2));
		Core.merge(channels, result);
		for (Mat channel : channels)
			channel.release();
		return new Img(result, false);
	}

	public Img resize() {
		Mat result = new Mat();
		Imgproc.resize(src, result, new Size(50, 50), 10, 10, Imgproc.INTER_AREA);
		return new Img(result, false);
	}

	public Img equalizeHistoAdaptative() {
		CLAHE clahe = Imgproc.createCLAHE(2.0, new Size(8, 8));
		Mat result = new Mat();
		List<Mat> channels = new ArrayList<>();
		Core.split(src, channels);
		clahe.apply(channels.get(0), channels.get(0));
		clahe.apply(channels.get(1), channels.get(1));
		clahe.apply(channels.get(2), channels.get(2));
		Core.merge(channels, result);
		for (Mat channel : channels)
			channel.release();
		return new Img(result, false);
	}

	public Img equalizeHistoAdaptative2() {

		return equalizeHistoAdaptative(2.0, new Size(8, 8));
	}

	public Img adaptativeMeanThreshold() {
		return adaptativeMeanThreshold(11, 2);
	}

	public Img adaptativeGaussianThreshold() {
		return adaptativeGaussianThreshold(11, 2);
	}

	public Img niblackThreshold() {
		return niblackThreshold(17, 0.1);
	}

	public Img sauvolaThreshold() {
		return sauvolaThreshold(17, 0.4);
	}

	public Img nickThreshold() {
		return nickThreshold(11, 0.1);
	}

	public Img wolfThreshold() {
		return wolfThreshold(11, 0.3);
	}

	// Equalize histograms using a Contrast Limited Adaptive Histogram
	// Equalization algorithm
	public Img equalizeHistoAdaptative(double clipLimit, Size titleGridSize) {
		Mat result = new Mat();
		Mat channelL = new Mat();
		CLAHE clahe = Imgproc.createCLAHE(clipLimit, titleGridSize);
		Core.extractChannel(result, channelL, 0);
		clahe.apply(channelL, channelL);
		Core.insertChannel(channelL, result, 0);
		Imgproc.cvtColor(result, result, Imgproc.COLOR_Lab2BGR);
		channelL.release();
		return new Img(result, false);
	}

	public Img otsuAfterGaussianBlur(Size blurSize) {
		// Same as otsu filtering, but a Gaussian blur is applied first
		return bgr2Gray().gaussianBlur(blurSize).thresHold(0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
	}

	public Img sobel(int ddepth, int dx, int dy, int ksize, double scale, double delta, int borderType) {
		Mat result = new Mat();
		Imgproc.Sobel(src, result, ddepth, dx, dy, ksize, scale, delta, borderType);
		return new Img(result, false);
	}

	public Img adaptativeThresHold(double maxValue, int adaptiveMethod, int thresholdType, int blockSize, double C) {
		Mat result = new Mat();
		Imgproc.adaptiveThreshold(src, result, maxValue, adaptiveMethod, thresholdType, blockSize, C);
		return new Img(result, false);
	}

	public Img thresHold(double thresh, double maxval, int type) {
		Mat result = new Mat();
		Imgproc.threshold(src, result, thresh, maxval, type);
		return new Img(result, false);
	}

	// TODO: make it faster (compute with integral image?)
	public Img bernsen(int ksize, int contrast_limit) {
		Img gray = bgr2Gray();
		Mat ret = Mat.zeros(gray.size(), gray.type());
		for (int i = 0; i < gray.cols(); i++) {
			for (int j = 0; j < gray.rows(); j++) {
				double mn = 999, mx = 0;
				int ti = 0, tj = 0;
				int tlx = i - ksize / 2;
				int tly = j - ksize / 2;
				int brx = i + ksize / 2;
				int bry = j + ksize / 2;
				if (tlx < 0)
					tlx = 0;
				if (tly < 0)
					tly = 0;
				if (brx >= gray.cols())
					brx = gray.cols() - 1;
				if (bry >= gray.rows())
					bry = gray.rows() - 1;

				for (int ik = -ksize / 2; ik <= ksize / 2; ik++) {
					for (int jk = -ksize / 2; jk <= ksize / 2; jk++) {
						ti = i + ik;
						tj = j + jk;
						if (ti > 0 && ti < gray.cols() && tj > 0 && tj < gray.rows()) {
							double pix = gray.get(tj, ti)[0];
							if (pix < mn)
								mn = pix;
							if (pix > mx)
								mx = pix;
						}
					}
				}
				double median = 0.5 * (mn + mx);
				if (median < contrast_limit) {
					ret.put(j, i, 0);
				} else {
					double pix = gray.get(j, i)[0];
					ret.put(j, i, pix > median ? 255 : 0);
				}
			}
		}
		return new Img(ret, false);
	}

	public int rows() {
		return src.rows();
	}

	public int cols() {
		return src.cols();
	}

	public Img dilateBlacks(double valueThreshold, double saturatioThreshold, double blueThreshold, Size dilatation) {
		return range(new Scalar(0, 0, 0), new Scalar(255, saturatioThreshold, valueThreshold), true).range(new Scalar(0, 0, 0), new Scalar(blueThreshold, 255, 255), false).morphologyEx(Imgproc.MORPH_DILATE, Imgproc.MORPH_RECT, dilatation);
	}

	public Img adaptativeMeanThreshold(int blockSize, double C) {
		return bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, blockSize, C);
	}

	public Img adaptativeGaussianThreshold(int blockSize, double C) {
		return bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY, blockSize, C);
	}

	public Img adaptativeGaussianInvThreshold(int blockSize, double C) {
		return bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, blockSize, C);
	}

	public Img niblackThreshold(int blockSize, double k) {
		Mat result = new Mat();
		Ximgproc.niBlackThreshold(bgr2Gray().getSrc(), result, 255, Imgproc.THRESH_BINARY, blockSize, k, Ximgproc.BINARIZATION_NIBLACK);
		return new Img(result, false);
	}

	public Img sauvolaThreshold(int blockSize, double k) {
		Mat result = new Mat();
		Ximgproc.niBlackThreshold(bgr2Gray().getSrc(), result, 255, Imgproc.THRESH_BINARY_INV, blockSize, k, Ximgproc.BINARIZATION_SAUVOLA);
		return new Img(result, false);
	}

	public Img nickThreshold(int blockSize, double k) {
		Mat result = new Mat();
		Ximgproc.niBlackThreshold(bgr2Gray().getSrc(), result, 255, Imgproc.THRESH_BINARY, blockSize, k, Ximgproc.BINARIZATION_NICK);
		return new Img(result, false);
	}

	public Img wolfThreshold(int blockSize, double k) {
		Mat result = new Mat();
		Ximgproc.niBlackThreshold(bgr2Gray().getSrc(), result, 255, Imgproc.THRESH_BINARY_INV, blockSize, k, Ximgproc.BINARIZATION_WOLF);
		return new Img(result, false);
	}

	public Img resize(Size size) {
		Mat result = new Mat();
		Imgproc.resize(src, result, size);
		return new Img(result, false);
	}

	public Img resize(double coeff) {
		Mat result = new Mat();
		Imgproc.resize(src, result, new Size(src.width() * coeff, src.height() * coeff));
		return new Img(result, false);
	}

	public Img bilateralFilter(int d, double sigmaColor, double sigmaSpace) {
		Mat result = new Mat();
		Imgproc.bilateralFilter(src, result, d, sigmaColor, sigmaSpace);
		return new Img(result, false);
	}

	public Img bilateralFilter() {
		return bilateralFilter(5, 80, 80);
	}

	public Img distanceTransform() {
		Mat result = new Mat();
		Imgproc.distanceTransform(src, result, Imgproc.DIST_L2, 5);
		return new Img(result, false);
	}

	public Img absDiff(Img img) {
		Mat result = new Mat();
		Core.absdiff(src, img.getSrc(), result);
		return new Img(result, false);
	}

	public Img hsvChannel(int channel) {
		Mat result = new Mat();
		Imgproc.cvtColor(src, result, Imgproc.COLOR_BGR2HSV);
		List<Mat> channels = new ArrayList<>();
		Core.split(result, channels);
		return new Img(channels.get(channel), false);
	}

	public Img bgrChannel(int channel) {
		List<Mat> channels = new ArrayList<>();
		Core.split(src, channels);
		return new Img(channels.get(channel), false);
	}

	public Img eraseCorners(double proportion) {
		Img result = new Img(src);
		int width = Double.valueOf(src.width() * proportion).intValue();
		int height = Double.valueOf(src.height() * proportion).intValue();
		Mat roi = new Mat(result.getSrc(), new Rect(0, 0, width, height));
		roi.setTo(new Scalar(255, 255, 255));
		roi = new Mat(result.getSrc(), new Rect(0, src.height() - height, width, height));
		roi.setTo(new Scalar(255, 255, 255));
		roi = new Mat(result.getSrc(), new Rect(src.width() - width, src.height() - height, width, height));
		roi.setTo(new Scalar(255, 255, 255));
		roi = new Mat(result.getSrc(), new Rect(src.width() - width, 0, width, height));
		roi.setTo(new Scalar(255, 255, 255));
		return result;
	}

	public Img fastNlMeansDenoising() {
		Mat result = new Mat();
		Photo.fastNlMeansDenoising(src, result);
		return new Img(result, false);
	}

	public int findBestHisto(List<Img> imgs) {

		List<Map<Integer, Double>> results = new ArrayList<>();
		for (Img img : imgs)
			results.add(compareHistogramm(computeHistogramm(), img));

		List<Integer> methods = Arrays.asList(Imgproc.HISTCMP_CORREL, Imgproc.HISTCMP_CHISQR, Imgproc.HISTCMP_INTERSECT, Imgproc.HISTCMP_BHATTACHARYYA, Imgproc.HISTCMP_CHISQR_ALT, Imgproc.HISTCMP_KL_DIV);
		Map<Integer, Integer> mins = new HashMap<>();
		for (Integer method : methods) {
			double min = results.get(0).get(method);
			int index = 0;
			for (int i = 0; i < results.size(); i++) {
				if (min > results.get(i).get(method)) {
					min = results.get(i).get(method);
					index = i;
					// System.out.println("method=" + method + " index=" +
					// index);
				}
			}
			mins.put(index, mins.get(index) != null ? mins.get(index) + 1 : 1);
		}
		TreeMap<Integer, Integer> reverse = mins.entrySet().stream().collect(Collectors.toMap(entry -> entry.getValue(), entry -> entry.getKey(), (u, v) -> {
			return u;
		}, TreeMap::new));
		// System.out.println("Number of algos : " +
		// reverse.lastEntry().getKey());
		return reverse.lastEntry().getValue();

	}

	public Mat computeHistogramm() {
		MatOfInt channels = new MatOfInt(0, 1, 2);
		MatOfInt histSize = new MatOfInt(8, 8, 8);
		MatOfFloat ranges = new MatOfFloat(0, 256, 0, 256, 0, 256);
		Mat rgb = cvtColor(Imgproc.COLOR_BGR2RGB).getSrc();
		Mat hist = new Mat();
		Imgproc.calcHist(Arrays.asList(rgb), channels, Mat.ones(rgb.size(), CvType.CV_8UC1), hist, histSize, ranges);
		// Core.normalize(hist, hist, 0, 1, Core.NORM_MINMAX, -1, new Mat());
		Core.normalize(hist, hist);
		return hist;
	}

	public Map<Integer, Double> compareHistogramm(Mat histo, Img img) {
		Map<Integer, Double> results = new HashMap<>();
		List<Integer> methods = Arrays.asList(Imgproc.HISTCMP_CORREL, Imgproc.HISTCMP_CHISQR, Imgproc.HISTCMP_INTERSECT, Imgproc.HISTCMP_BHATTACHARYYA, Imgproc.HISTCMP_CHISQR_ALT, Imgproc.HISTCMP_KL_DIV);
		for (int method : methods) {
			double result = Imgproc.compareHist(histo, img.computeHistogramm(), method);
			switch (method) {
			case Imgproc.HISTCMP_CORREL:
				result = -result;
				break;
			case Imgproc.HISTCMP_INTERSECT:
				result = -result;
				break;
			}
			results.put(method, result);
			// System.out.println("for Algo " + method + " comparison : " +
			// result + "\n");
		}
		// System.out.println("results : " + results);
		return results;

	}

	public List<Double> projectVertically() {
		Mat result = new Mat();
		Core.reduce(getSrc(), result, 1, Core.REDUCE_AVG, CvType.CV_64F);
		List<Double> histoVertical = new ArrayList<>();
		Converters.Mat_to_vector_double(result, histoVertical);
		return histoVertical;
	}

	public List<Double> projectHorizontally() {
		Mat result = new Mat();
		Core.reduce(getSrc(), result, 0, Core.REDUCE_AVG, CvType.CV_64F);
		Core.transpose(result, result);
		List<Double> histoHorizontal = new ArrayList<>();
		Converters.Mat_to_vector_double(result, histoHorizontal);
		return histoHorizontal;
	}

	private Img range(Scalar scalar, Scalar scalar2) {
		Mat result = new Mat();
		Core.inRange(getSrc(), scalar, scalar2, result);
		return new Img(result, false);
	}

	public Img add(Img img) {
		Mat result = new Mat();
		Core.add(getSrc(), img.getSrc(), result);
		return new Img(result, false);
	}

	public Img bitwise_and(Img img) {
		Mat result = new Mat();
		Core.bitwise_and(getSrc(), img.getSrc(), result);
		return new Img(result, false);
	}

	public Img bitwise_or(Img img) {
		Mat result = new Mat();
		Core.bitwise_or(getSrc(), img.getSrc(), result);
		return new Img(result, false);
	}

	public Img bitwise_xor(Img img) {
		Mat result = new Mat();
		Core.bitwise_xor(getSrc(), img.getSrc(), result);
		return new Img(result, false);
	}

	public Img bitwise_not() {
		Mat result = new Mat();
		Core.bitwise_not(getSrc(), result);
		return new Img(result, false);
	}

	public Img transpose() {
		Mat result = new Mat();
		Core.transpose(src, result);
		return new Img(result, false);
	}

	public Img houghLinesP(double rho, double theta, int threshold) {
		Mat result = new Mat();
		Imgproc.HoughLinesP(src, result, rho, theta, threshold);
		return new Img(result, false);
	}

	public Img houghLinesP(int rho, double theta, int threshold, double mineLineLenght, double maxLineGap) {
		Mat result = new Mat();
		Imgproc.HoughLinesP(src, result, rho, theta, threshold, mineLineLenght, maxLineGap);
		return new Img(result, false);
	}

	public Image toJfxImage() {
		return Tools.mat2jfxImage(src);
	}

	public ImageView toJfxImageView() {
		return new ImageView(toJfxImage());
	}

	public MatOfFloat getHogDescriptor() {
		MatOfFloat imgDescriptor = new MatOfFloat();
		HOGDescriptor hog = new HOGDescriptor(new Size(64, 64), new Size(16, 16), new Size(16, 16), new Size(16, 16), 4);
		hog.compute(src, imgDescriptor);
		return imgDescriptor;
	}

	@Override
	public void close() {
		src.release();
	}

	public Layout buildLayout() {
		return this.buildLayout(new Size(0.04, 0.008), 5);
	}

	public Layout buildLayout(Size morph, int level) {
		Layout root = new Layout(null, 0, 1, 0, 1).tighten(this);
		return root.recursiveSplit(morph, level, root.getRoi(this));
	}

	private static boolean[] close(List<Boolean> histo, int k) {
		boolean[] closed = new boolean[histo.size()];
		for (int i = 0; i < histo.size() - 1; i++)
			if (histo.get(i) && !histo.get(i + 1)) {
				for (int j = k + 1; j > 0; j--)
					if (i + j < histo.size()) {
						if (histo.get(i + j)) {
							Arrays.fill(closed, i, i + j + 1, true);
							i += j - 1;
							break;
						}
						closed[i] = histo.get(i);
					}
			} else
				closed[i] = histo.get(i);
		if (!closed[histo.size() - 1])
			closed[histo.size() - 1] = histo.get(histo.size() - 1);
		return closed;
	}

	private List<Boolean> getRow(int row) {
		List<Boolean> result = new ArrayList<>(src.cols());
		for (int col = 0; col < src.cols(); col++)
			result.add(src.get(row, col)[0] >= 255.0);
		return result;
	};

	public List<Boolean> getCol(int col) {
		List<Boolean> result = new ArrayList<>(src.rows());
		for (int row = 0; row < src.rows(); row++)
			result.add(src.get(row, col)[0] >= 255.0);
		return result;
	};

	public Img cleanTables(double close) {
		Mat result = new Mat(src.size(), CvType.CV_8U, new Scalar(255));
		boolean[][] hHistos = new boolean[src.cols()][src.rows()];
		for (int col = 0; col < src.cols(); col++)
			hHistos[col] = close(getCol(col), Long.valueOf(Math.round(close * src.rows())).intValue());
		boolean[][] vHistos = new boolean[src.rows()][src.cols()];
		for (int row = 0; row < src.rows(); row++)
			vHistos[row] = close(getRow(row), Long.valueOf(Math.round(close * src.cols())).intValue());
		for (int col = 0; col < src.cols(); col++)
			for (int row = 0; row < src.rows(); row++)
				result.put(row, col, (src.get(row, col)[0] >= 255.0) ^ !(hHistos[col][row] && vHistos[row][col]) ? 255.0 : 0.0);
		return new Img(result, false);

		// Img hImg = this.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(close * getSrc().width(), 1));
		// Img vImg = this.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(1, close * getSrc().height()));
		// return new Img(this.bitwise_xor(hImg.bitwise_and(vImg).bitwise_not()).getSrc());
	}

	public Img cleanFaces(double px, double py) {
		Img result = new Img(getSrc());
		Rect[] faces = FaceDetector.detect(result.getSrc());
		Arrays.stream(faces).forEach(face -> new Zone(0, face).adjustRect(result.getSrc().width() * px / 2, result.getSrc().height() * py / 2, result.getSrc().width(), result.getSrc().height()).draw(result, Scalar.all(0), -1));
		return result;
	}
}
