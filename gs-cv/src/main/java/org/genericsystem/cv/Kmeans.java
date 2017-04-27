package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.TermCriteria;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;

public class Kmeans {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}
	private static VideoCapture camera = new VideoCapture(0);

	private final static int MAX_ITER = 10;
	private final static int CLUSTERS = 8;

	public static void main(String[] args) {
		JFrame jframe = new JFrame("Kmeans");
		jframe.setResizable(false);
		jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JLabel vidpanel = new JLabel();
		jframe.setContentPane(vidpanel);

		Mat img = new Mat();
		camera.read(img);
		jframe.setSize(img.width(), img.height());
		jframe.setVisible(true);
		for (;;) {
			camera.read(img);
			Mat clusters = colorMapKMeans(img, CLUSTERS, MAX_ITER);

			ImageIcon image = new ImageIcon(mat2bufferedImage(clusters));

			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

	public static Mat colorMapKMeans(Mat img, int cluster) {
		return colorMapKMeans(img, cluster, MAX_ITER);
	}

	public static Mat colorMapKMeans(Mat img, int K, int maxIterations) {

		Mat m = img.reshape(1, img.rows() * img.cols());
		m.convertTo(m, CvType.CV_32F);

		Mat bestLabels = new Mat(m.rows(), 1, CvType.CV_8U);
		Mat centroids = new Mat(K, 1, CvType.CV_32F);
		Core.kmeans(m, K, bestLabels, new TermCriteria(TermCriteria.COUNT | TermCriteria.EPS, maxIterations, 1E-5), 1, Core.KMEANS_RANDOM_CENTERS, centroids);
		List<Integer> idx = new ArrayList<>(m.rows());
		Converters.Mat_to_vector_int(bestLabels, idx);

		Mat imgMapped = new Mat(m.size(), m.type());
		for (int i = 0; i < idx.size(); i++) {
			Mat row = imgMapped.row(i);
			centroids.row(idx.get(i)).copyTo(row);
		}
		Mat result = new Mat();
		Mat mat32 = imgMapped.reshape(3, img.rows());
		mat32.convertTo(result, CvType.CV_8U);
		return result;
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