package org.genericsystem.cv;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.BackgroundSubtractorKNN;
import org.opencv.video.BackgroundSubtractorMOG2;
import org.opencv.video.Video;
import org.opencv.videoio.VideoCapture;

public class BackGroundSubstractor {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private static VideoCapture camera = new VideoCapture(0);
	private final static String adjustedDirectoryPath2 = "aligned-image-3.png";

	private static List<Mat> getClassMats2() {
		return Arrays.stream(new File(adjustedDirectoryPath2).listFiles()).filter(img -> img.getName().endsWith(".png")).map(img -> Imgcodecs.imread(img.getPath())).collect(Collectors.toList());
	}

	public static void main(String[] args) {

		JFrame jframe = new JFrame("BackgroundSubstractor");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);

		List<Mat> srcs = getClassMats2();
		Mat img = new Mat();
		srcs.get(0).copyTo(img);
		jframe.setSize(img.width(), img.height());
		jframe.setVisible(true);
		BackgroundSubtractorMOG2 mog2 = Video.createBackgroundSubtractorMOG2();
		BackgroundSubtractorKNN knn = Video.createBackgroundSubtractorKNN();

		for (int i = 0; i < 500; i++) {

			srcs.get(i % srcs.size()).copyTo(img);
			// camera.read(img);

			Mat gray = new Mat();
			Imgproc.cvtColor(img, gray, Imgproc.COLOR_BGR2GRAY);
			Mat fgMask = new Mat();
			knn.apply(gray, fgMask);
			Mat fg = new Mat();
			img.copyTo(fg, fgMask);
			ImageIcon image = new ImageIcon(Tools.mat2bufferedImage(fg));

			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

	public Mat createMask(Mat frame, Mat backGround) {
		// copy as we are going to destruct those images maybe
		Mat camBlur = frame.clone();
		int blurx = 3;
		int blury = 3;
		int subStrationThreshold = 100;
		// remove noise
		Imgproc.blur(camBlur, camBlur, new Size(blurx, blury));

		// take abs diff and create binary image in all 3 channels
		Mat diff = new Mat();
		Core.absdiff(backGround, camBlur, diff);
		Imgproc.threshold(diff, diff, subStrationThreshold, 255, Imgproc.THRESH_BINARY);

		List<Mat> channels = new ArrayList<Mat>();
		Core.split(diff, channels);
		Mat mask = channels.get(2).clone();
		Core.add(mask, channels.get(1), mask);
		Core.add(mask, channels.get(0), mask);

		// dilate to remove some black gaps within balls
		Imgproc.dilate(mask, mask, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(3, 3)));

		return mask;
	}

	// private Mat equalizeHisto(Mat mat) {
	// Mat result = new Mat();
	// Imgproc.cvtColor(mat, result, Imgproc.COLOR_BGR2YCrCb);
	// List<Mat> channels = new ArrayList<Mat>();
	// Core.split(result, channels);
	// Imgproc.equalizeHist(channels.get(0), channels.get(0));
	// // Imgproc.equalizeHist(channels.get(1), channels.get(1));
	// // Imgproc.equalizeHist(channels.get(2), channels.get(2));
	// Core.merge(channels, result);
	// Imgproc.cvtColor(result, result, Imgproc.COLOR_YCrCb2BGR);
	// return result;
	// }
}
