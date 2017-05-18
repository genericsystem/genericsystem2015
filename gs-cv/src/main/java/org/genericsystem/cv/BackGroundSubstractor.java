package org.genericsystem.cv;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.video.BackgroundSubtractorKNN;
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
		// srcs.get(0).copyTo(img);
		camera.read(img);
		jframe.setSize(img.width(), img.height());
		jframe.setVisible(true);
		// BackgroundSubtractorMOG2 mog2 = Video.createBackgroundSubtractorMOG2();
		BackgroundSubtractorKNN knn = Video.createBackgroundSubtractorKNN();
		Mat average = null;
		for (int i = 1; i < 500; i++) {

			// srcs.get(i % srcs.size()).copyTo(img);
			camera.read(img);

			Mat fgMask = new Mat();
			knn.apply(img, fgMask);
			Mat fg = new Mat();
			img.copyTo(fg, fgMask);
			if (average == null)
				average = fg;
			else
				Core.addWeighted(average, 1 - 1d / 30, fg, 1d / 30, 0, average);
			ImageIcon image = new ImageIcon(Tools.mat2bufferedImage(average));

			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

}
