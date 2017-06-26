package org.genericsystem.cv;

import java.io.File;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.core.TermCriteria;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.ml.Ml;
import org.opencv.ml.SVM;

public class Svm {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}
	private final static String imgClassDirectory = "tmp";

	public static void main(String[] args) {
		Mat classes = new Mat();
		Mat trainingData = new Mat();

		Mat trainingImages = new Mat();
		Mat trainingLabels = new Mat();

		SVM clasificador;

		Size size = new Size(128, 128);

		for (File file : new File(imgClassDirectory + "/positives/").listFiles()) {
			if (file.getName().endsWith(".png")) {

				Mat img = Imgcodecs.imread(file.getPath());
				Imgproc.resize(img, img, size);
				img = img.reshape(1, 1);

				trainingImages.push_back(img);
				trainingLabels.push_back(Mat.ones(new Size(1, 1), CvType.CV_32S));
			}
		}

		for (File file : new File(imgClassDirectory + "/negatives/").listFiles()) {
			if (file.getName().endsWith(".png")) {
				Mat img = Imgcodecs.imread(file.getPath());
				Imgproc.resize(img, img, size);
				img = img.reshape(1, 1);

				trainingImages.push_back(img);
				trainingLabels.push_back(Mat.zeros(new Size(1, 1), CvType.CV_32S));
			}
		}

		trainingImages.convertTo(trainingData, CvType.CV_32FC1);
		trainingLabels.copyTo(classes);

		clasificador = SVM.create();
		clasificador.setType(SVM.C_SVC);
		clasificador.setTermCriteria(new TermCriteria(TermCriteria.MAX_ITER, 100, 1e-6));
		clasificador.setKernel(SVM.LINEAR);
		clasificador.train(trainingData, Ml.ROW_SAMPLE, trainingLabels);

		for (File file : new File(imgClassDirectory + "/samples/").listFiles()) {
			if (file.getName().endsWith(".png")) {
				Mat img = Imgcodecs.imread(file.getPath());
				Imgproc.resize(img, img, size);
				img = img.reshape(1, 1);
				img.convertTo(img, CvType.CV_32FC1);
				System.out.println(file.getName() + " : " + clasificador.predict(img));
			}
		}
	}
}
