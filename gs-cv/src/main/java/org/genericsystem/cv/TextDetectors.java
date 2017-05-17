package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;

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

public class TextDetectors {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private static Scalar white = new Scalar(255, 255, 255);
	private static Scalar black = new Scalar(0, 0, 0);
	private static Scalar green = new Scalar(0, 255, 0);
	private static Scalar blue = new Scalar(255, 0, 0);
	private static Scalar red = new Scalar(0, 0, 255);
	private static Scalar cyan = new Scalar(255, 255, 0);

	public static void main(String[] args) {
		JFrame jframe = new JFrame("Mserdetector");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);
		VideoCapture camera = new VideoCapture(0);
		Mat frame = new Mat();
		camera.read(frame);
		jframe.setSize(frame.width(), frame.height());
		jframe.setVisible(true);
		for (;;) {
			camera.read(frame);
			Img frameImg = new Img(frame);
			Img result = new Img(frameImg.getSrc());

			Zones.get(frameImg.sobel(), 400).draw(result, new Scalar(0, 255, 0), 1);
			// Zones.get(frameImg.classic(), 400).draw(result, new Scalar(255, 255, 0), 2);
			Zones.get(frameImg.grad(), 400).draw(result, new Scalar(0, 0, 255), 1);
			Zones.get(frameImg.mser(), 400).draw(result, new Scalar(255, 0, 0), 1);

			vidpanel.setIcon(result.getImageIcon());
			vidpanel.repaint();
		}

	}

	public static void mserDetectText(Img frame) {
		Img gray = frame.cvtColor(Imgproc.COLOR_BGR2GRAY);
		MatOfKeyPoint keypoint = new MatOfKeyPoint();
		FeatureDetector detector = FeatureDetector.create(FeatureDetector.MSER);
		detector.detect(gray.getSrc(), keypoint);
		List<KeyPoint> listpoint = keypoint.toList();
		Mat mask = Mat.zeros(gray.size(), CvType.CV_8UC1);
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
			Mat roi = new Mat(mask, rectant);
			roi.setTo(new Scalar(255));
		}

		int imgsize = gray.height() * gray.width();
		Mat morbyte = new Mat();
		Mat hierarchy = new Mat();
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.morphologyEx(mask, morbyte, Imgproc.MORPH_DILATE, new Mat(1, 50, CvType.CV_8UC1, new Scalar(255)));
		Imgproc.findContours(morbyte, contours, hierarchy, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (int i = 0; i < contours.size(); i++) {
			Rect rect = Imgproc.boundingRect(contours.get(i));
			if (rect.area() > 0.5 * imgsize || rect.area() < 100 || rect.width / rect.height < 2) {
				Mat roi = new Mat(morbyte, rect);
				roi.setTo(black);
			} else
				frame.rectangle(rect, green, 1);
		}
	}

	public static void lpdDetectText(Img frame) {
		Img gray = frame.cvtColor(Imgproc.COLOR_BGR2GRAY);
		Img sobel = gray.sobel(CvType.CV_8UC1, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT);
		Img threshold = sobel.thresHold(0, 255, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		Img connected = threshold.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 3)));
		List<MatOfPoint> contours = connected.findContours(new Img[1], Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (int i = 0; i < contours.size(); i++) {
			if (Imgproc.contourArea(contours.get(i)) > 400) {
				Rect rect = Imgproc.boundingRect(contours.get(i));
				if (rect.width > rect.height)
					frame.rectangle(rect, red, 2);
			}
		}
	}

	// MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
	// Point[] result = new Point[4];
	// Imgproc.minAreaRect(contour2F).points(result);
	// Imgproc.drawContours(frame, Arrays.asList(new MatOfPoint(result)), 0, new Scalar(255, 0, 0), 2);

	public static void morphGradientDetectText(Img frame) {
		Img gray = frame.cvtColor(Imgproc.COLOR_BGR2GRAY);
		Img grad = gray.morphologyEx(Imgproc.MORPH_GRADIENT, new StructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));
		Img threshold = grad.thresHold(0.0, 255.0, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		Img connected = threshold.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(9, 1)));
		Mat mask = new Mat(threshold.size(), CvType.CV_8UC1, black);
		Img[] hierarchy = new Img[1];
		List<MatOfPoint> contours = connected.findContours(hierarchy, Imgproc.RETR_CCOMP, Imgproc.CHAIN_APPROX_SIMPLE, new Point(0, 0));
		for (int i = 0; i >= 0; i = (int) hierarchy[0].get(0, i)[0]) {
			Rect rect = Imgproc.boundingRect(contours.get(i));
			Mat maskROI = new Mat(mask, rect);
			maskROI.setTo(black);
			// fill the contour
			Imgproc.drawContours(mask, contours, i, white, Core.FILLED);
			// ratio of non-zero pixels in the filled region
			double r = Integer.valueOf(Core.countNonZero(maskROI)).doubleValue() / (rect.width * rect.height);

			if (r > .45 /* assume at least 45% of the area is filled if it contains text */
					&& (rect.height > 8 && rect.width > 8) /* constraints on region size */
			/*
			 * these two conditions alone are not very robust. better to use something like the number of significant peaks in a horizontal projection as a third condition
			 */
			) {
				frame.rectangle(rect, blue, 2);
			}
		}

	}

	public static void anotherDetector(Img frame) {
		Img gray = frame.cvtColor(Imgproc.COLOR_BGR2GRAY);
		Img sobel = gray.sobel(CvType.CV_8U, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT);
		Img threshold = sobel.thresHold(0, 255, Imgproc.THRESH_OTSU + Imgproc.THRESH_BINARY);
		threshold = threshold.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(17, 3)));
		List<MatOfPoint> contours = threshold.findContours(new Img[1], Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_NONE);
		MatOfPoint2f mMOP2f1, mMOP2f2;
		mMOP2f1 = new MatOfPoint2f();
		mMOP2f2 = new MatOfPoint2f();
		for (int i = 0; i < contours.size(); i++)
			if (contours.get(i).toList().size() > 100) {
				contours.get(i).convertTo(mMOP2f1, CvType.CV_32FC2);
				Imgproc.approxPolyDP(mMOP2f1, mMOP2f2, 3, true);
				mMOP2f2.convertTo(contours.get(i), CvType.CV_32S);
				Rect appRect = Imgproc.boundingRect(contours.get(i));
				if (appRect.width > appRect.height)
					frame.rectangle(appRect, cyan, 1);
			}
	}

	public static void anotherMserDetector(Img frame) {
		Img gray = frame.cvtColor(Imgproc.COLOR_BGR2GRAY);
		FeatureDetector fd = FeatureDetector.create(FeatureDetector.MSER);
		MatOfKeyPoint mokp = new MatOfKeyPoint();
		Img edges = gray.canny(50, 200);
		fd.detect(gray.getSrc(), mokp, edges.getSrc());
		Features2d.drawKeypoints(frame.getSrc(), mokp, frame.getSrc());
	}
}
