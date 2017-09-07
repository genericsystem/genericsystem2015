package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.genericsystem.cv.utils.NativeLibraryLoader;
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
		NativeLibraryLoader.load();
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
			Mat clusters = colorMapKMeans(img, CLUSTERS);
			// Mat clusters = cluster(img, CLUSTERS).get(1);

			ImageIcon image = new ImageIcon(mat2bufferedImage(clusters));

			vidpanel.setIcon(image);
			vidpanel.repaint();
		}

	}

	public static Mat colorMapKMeans(Mat img, int K) {

		Mat m = img.reshape(1, img.rows() * img.cols());
		m.convertTo(m, CvType.CV_32F);

		Mat bestLabels = new Mat(m.rows(), 1, CvType.CV_8U);
		Mat centroids = new Mat(K, 1, CvType.CV_32F);
		Core.kmeans(m, K, bestLabels, new TermCriteria(TermCriteria.COUNT | TermCriteria.EPS, MAX_ITER, 1E-5), 1, Core.KMEANS_PP_CENTERS, centroids);
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

	public static List<Mat> cluster(Mat cutout, int k) {
		Mat samples = cutout.reshape(1, cutout.cols() * cutout.rows());
		Mat samples32f = new Mat();
		samples.convertTo(samples32f, CvType.CV_32F, 1.0 / 255.0);
		Mat labels = new Mat();
		Mat centers = new Mat();
		Core.kmeans(samples32f, k, labels, new TermCriteria(TermCriteria.COUNT | TermCriteria.EPS, MAX_ITER, 1E-5), 1, Core.KMEANS_PP_CENTERS, centers);

		// Objdetect.groupRectangles(rectList, weights, groupThreshold);

		return showClusters(cutout, labels, centers);
	}

	private static List<Mat> showClusters(Mat cutout, Mat labels, Mat centers) {
		centers.convertTo(centers, CvType.CV_8UC1, 255);
		centers.reshape(3);
		List<Mat> clusters = new ArrayList<>();
		for (int i = 0; i < centers.rows(); i++) {
			clusters.add(Mat.zeros(cutout.size(), cutout.type()));
		}
		Map<Integer, Integer> counts = new HashMap<>();
		for (int i = 0; i < centers.rows(); i++)
			counts.put(i, 0);
		int rows = 0;
		for (int y = 0; y < cutout.rows(); y++) {
			for (int x = 0; x < cutout.cols(); x++) {
				int label = (int) labels.get(rows, 0)[0];
				int r = (int) centers.get(label, 2)[0];
				int g = (int) centers.get(label, 1)[0];
				int b = (int) centers.get(label, 0)[0];
				clusters.get(label).put(y, x, b, g, r);
				rows++;
			}
		}
		return clusters;
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