package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.FeatureDetector;
import org.opencv.features2d.Features2d;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class MserDetector {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private static VideoCapture camera = new VideoCapture(0);

	static boolean read(Mat frame) {
		return camera.read(frame);
	}

	public static void main(String[] args) {
		JFrame jframe = new JFrame("Mserdetector");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);
		Mat frame = new Mat();
		Mat grey = new Mat();
		read(frame);
		jframe.setSize(frame.width(), frame.height());
		jframe.setVisible(true);
		for (;;) {
			read(frame);
			Imgproc.cvtColor(frame, grey, Imgproc.COLOR_BGR2GRAY);
			// anotherMserDetector(frame, grey);
			mserDetectText(frame, grey);
			lpdDetectText(frame, grey);
			morphGradientDetectText(frame, grey);
			ImageIcon image = new ImageIcon(Tools.mat2bufferedImage(frame));
			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

	public static void mserDetectText(Mat frame, Mat gray) {
		MatOfKeyPoint keypoint = new MatOfKeyPoint();
		List<KeyPoint> listpoint;
		KeyPoint kpoint;
		Mat mask = Mat.zeros(gray.size(), CvType.CV_8UC1);
		int rectanx1;
		int rectany1;
		int rectanx2;
		int rectany2;
		int imgsize = gray.height() * gray.width();
		Scalar white = new Scalar(255);

		List<MatOfPoint> contours = new ArrayList<>();
		Mat kernel = new Mat(1, 50, CvType.CV_8UC1, Scalar.all(255));
		Mat morbyte = new Mat();
		Mat hierarchy = new Mat();

		FeatureDetector detector = FeatureDetector.create(FeatureDetector.MSER);
		detector.detect(gray, keypoint);
		listpoint = keypoint.toList();

		for (int ind = 0; ind < listpoint.size(); ind++) {
			kpoint = listpoint.get(ind);
			rectanx1 = (int) (kpoint.pt.x - 0.5 * kpoint.size);
			rectany1 = (int) (kpoint.pt.y - 0.5 * kpoint.size);
			rectanx2 = (int) (kpoint.size);
			rectany2 = (int) (kpoint.size);
			if (rectanx1 <= 0)
				rectanx1 = 1;
			if (rectany1 <= 0)
				rectany1 = 1;
			if ((rectanx1 + rectanx2) > gray.width())
				rectanx2 = gray.width() - rectanx1;
			if ((rectany1 + rectany2) > gray.height())
				rectany2 = gray.height() - rectany1;
			Rect rectant = new Rect(rectanx1, rectany1, rectanx2, rectany2);
			Mat roi = new Mat(mask, rectant);
			roi.setTo(white);
		}

		Imgproc.morphologyEx(mask, morbyte, Imgproc.MORPH_DILATE, kernel);
		Imgproc.findContours(morbyte, contours, hierarchy, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (int i = 0; i < contours.size(); i++) {
			Rect rect = Imgproc.boundingRect(contours.get(i));
			if (rect.area() <= 0.5 * imgsize && rect.area() >= 100 && rect.width / rect.height > 2)
				Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(0, 255, 0));
		}
	}

	public static void lpdDetectText(Mat frame, Mat gray) {
		Mat grayCopy = gray.clone();
		Mat img_sobel = new Mat();
		Mat img_threshold = new Mat();
		Imgproc.Sobel(grayCopy, img_sobel, CvType.CV_8UC1, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT);
		Imgproc.threshold(img_sobel, img_threshold, 0, 255, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		Mat element = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(17, 3));
		Imgproc.morphologyEx(img_threshold, img_threshold, Imgproc.MORPH_CLOSE, element); // Does the trick
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(img_threshold, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_NONE);
		for (int i = 0; i < contours.size(); i++) {
			if (Imgproc.contourArea(contours.get(i)) > 100) {
				Rect rect = Imgproc.boundingRect(contours.get(i));
				if (rect.width > rect.height)
					Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(0, 0, 255));
			}
		}
	}

	// MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
	// Point[] result = new Point[4];
	// Imgproc.minAreaRect(contour2F).points(result);
	// Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 2);

	public static void morphGradientDetectText(Mat frame, Mat gray) {
		Mat grad = new Mat();

		// morphological gradient
		Mat morphKernel = Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		Imgproc.morphologyEx(gray, grad, Imgproc.MORPH_GRADIENT, morphKernel);

		// binarize
		Mat bw = new Mat();
		Imgproc.threshold(grad, bw, 0.0, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);

		// connect horizontally oriented regions
		Mat connected = new Mat();
		morphKernel = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(9, 1));
		Imgproc.morphologyEx(bw, connected, Imgproc.MORPH_CLOSE, morphKernel);

		Scalar zeos = new Scalar(0, 0, 0);
		Mat mask = new Mat(bw.size(), CvType.CV_8UC1, zeos);
		List<MatOfPoint> contours = new ArrayList<>();
		Mat hierarchy = new Mat();

		Imgproc.findContours(connected, contours, hierarchy, Imgproc.RETR_CCOMP, Imgproc.CHAIN_APPROX_SIMPLE, new Point(0, 0));
		// filter contours
		for (int idx = 0; idx >= 0; idx = (int) hierarchy.get(0, idx)[0]) {
			Rect rect = Imgproc.boundingRect(contours.get(idx));

			Mat maskROI = new Mat(mask, rect);
			maskROI.setTo(zeos);
			// fill the contour
			Imgproc.drawContours(mask, contours, idx, new Scalar(255d, 255d, 255d), Core.FILLED);
			// ratio of non-zero pixels in the filled region
			double r = Integer.valueOf(Core.countNonZero(maskROI)).doubleValue() / (rect.width * rect.height);

			if (r > .45 /* assume at least 45% of the area is filled if it contains text */
					&& (rect.height > 8 && rect.width > 8) /* constraints on region size */
			/*
			 * these two conditions alone are not very robust. better to use something like the number of significant peaks in a horizontal projection as a third condition
			 */
			) {
				Imgproc.rectangle(frame, rect.br(), rect.tl(), new Scalar(255, 0, 0), 1);
			}
		}

	}

	public void anotherDetector(Mat src, Mat dst) {
		if (dst != src) {
			src.copyTo(dst);
		}
		Mat img_gray, img_sobel, img_threshold, element;

		img_gray = new Mat();
		Imgproc.cvtColor(src, img_gray, Imgproc.COLOR_RGB2GRAY);

		img_sobel = new Mat();
		Imgproc.Sobel(img_gray, img_sobel, CvType.CV_8U, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT);

		img_threshold = new Mat();
		Imgproc.threshold(img_sobel, img_threshold, 0, 255, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);

		element = new Mat();
		element = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(17, 3));
		Imgproc.morphologyEx(img_threshold, img_threshold, Imgproc.MORPH_CLOSE, element);
		// Does the trick
		List<MatOfPoint> contours = new ArrayList<>();
		Mat hierarchy = new Mat();
		Imgproc.findContours(img_threshold, contours, hierarchy, 0, 1);
		List<MatOfPoint> contours_poly = new ArrayList<>(contours.size());
		contours_poly.addAll(contours);

		MatOfPoint2f mMOP2f1, mMOP2f2;
		mMOP2f1 = new MatOfPoint2f();
		mMOP2f2 = new MatOfPoint2f();

		for (int i = 0; i < contours.size(); i++)

			if (contours.get(i).toList().size() > 100) {
				contours.get(i).convertTo(mMOP2f1, CvType.CV_32FC2);
				Imgproc.approxPolyDP(mMOP2f1, mMOP2f2, 3, true);
				mMOP2f2.convertTo(contours_poly.get(i), CvType.CV_32S);
				Rect appRect = Imgproc.boundingRect(contours_poly.get(i));
				if (appRect.width > appRect.height) {
					Imgproc.rectangle(dst, new Point(appRect.x, appRect.y), new Point(appRect.x + appRect.width, appRect.y + appRect.height), new Scalar(255, 0, 0));
				}
			}

	}

	public static void anotherMserDetector(Mat frame, Mat gray) {
		FeatureDetector fd = FeatureDetector.create(FeatureDetector.MSER);
		MatOfKeyPoint mokp = new MatOfKeyPoint();
		Mat edges = new Mat();
		// for mask
		Imgproc.Canny(gray, edges, 400, 450);
		fd.detect(gray, mokp, edges);
		// for drawing keypoints

		Features2d.drawKeypoints(frame, mokp, frame);
	}
}
