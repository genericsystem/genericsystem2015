package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class CamCropper {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String refPath = "adjusted/image-3.png";

	private static VideoCapture camera = new VideoCapture(0);

	static boolean read(Mat frame) {
		return camera.read(frame);
	}

	private static Mat ref = Imgcodecs.imread(refPath);

	public static void main(String[] args) {

		JFrame jframe = new JFrame("CamCropper");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);

		Mat frame = new Mat();
		read(frame);
		jframe.setSize(ref.width(), ref.height());
		jframe.setVisible(true);
		Mat average = ref.clone();
		double n = 2;
		while (read(frame)) {
			Mat currentAdjustedFrame = adjust(frame);
			if (currentAdjustedFrame != null) {
				Core.addWeighted(average, (n - 1) / n, currentAdjustedFrame, 1d / n, 0, average);
				if (n < 5d)
					n++;
			}
			ImageIcon image = new ImageIcon(mat2bufferedImage(average));
			vidpanel.setIcon(image);
			vidpanel.repaint();
		}
	}

	public static Mat adjust(Mat frame) {
		Mat resized = new Mat();
		Imgproc.resize(frame, resized, new Size(frame.width() * 2, frame.height() * 2));
		return Classifier.compareFeature(resized, ref);
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
