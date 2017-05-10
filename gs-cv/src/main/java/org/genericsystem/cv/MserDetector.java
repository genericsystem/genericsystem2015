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
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.features2d.FeatureDetector;
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
			detectText(frame, grey);
			ImageIcon image = new ImageIcon(Tools.mat2bufferedImage(frame));
			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

	private static void detectText(Mat frame, Mat gray) {
		MatOfKeyPoint keypoint = new MatOfKeyPoint();
		List<KeyPoint> listpoint;
		KeyPoint kpoint;
		Mat mask = Mat.zeros(gray.size(), CvType.CV_8UC1);
		int rectanx1;
		int rectany1;
		int rectanx2;
		int rectany2;
		int imgsize = gray.height() * gray.width();
		Scalar zeos = new Scalar(0, 0, 0);

		List<MatOfPoint> contour2 = new ArrayList<MatOfPoint>();
		Mat kernel = new Mat(1, 50, CvType.CV_8UC1, Scalar.all(255));
		Mat morbyte = new Mat();
		Mat hierarchy = new Mat();

		Rect rectan3;
		//
		FeatureDetector detector = FeatureDetector.create(FeatureDetector.MSER);
		detector.detect(gray, keypoint);
		listpoint = keypoint.toList();
		//
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
			// Imgproc.rectangle(frame, rectant.br(), rectant.tl(), new Scalar(0, 255, 0));
			try {
				Mat roi = new Mat(mask, rectant);
				roi.setTo(new Scalar(255, 255, 255));
			} catch (Exception ex) {
				System.out.println("mat roi error " + ex.getMessage());
			}
		}

		Imgproc.morphologyEx(mask, morbyte, Imgproc.MORPH_DILATE, kernel);
		Imgproc.findContours(morbyte, contour2, hierarchy, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_NONE);
		for (int ind = 0; ind < contour2.size(); ind++) {
			rectan3 = Imgproc.boundingRect(contour2.get(ind));
			rectan3 = Imgproc.boundingRect(contour2.get(ind));
			if (rectan3.area() > 0.5 * imgsize || rectan3.area() < 100 || rectan3.width / rectan3.height < 2) {
				Mat roi = new Mat(morbyte, rectan3);
				roi.setTo(zeos);
			} else {
				Imgproc.rectangle(frame, rectan3.br(), rectan3.tl(), new Scalar(0, 255, 0));
			}
		}
	}

}
