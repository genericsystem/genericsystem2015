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

		Size size = new Size(16, 16);

		System.out.println(new File(imgClassDirectory + "/positives/").listFiles().length);
		System.out.println(new File(imgClassDirectory + "/negatives/").listFiles().length);

		for (File file : new File(imgClassDirectory + "/positives/").listFiles()) {
			if (file.getName().endsWith(".png")) {

				Mat img = Imgcodecs.imread(file.getPath());
				Imgproc.cvtColor(img, img, Imgproc.COLOR_BGR2GRAY);
				Imgproc.resize(img, img, size);
				img = img.reshape(1, 1);

				trainingImages.push_back(img);
				trainingLabels.push_back(Mat.ones(new Size(1, 1), CvType.CV_32S));
			}
		}

		for (File file : new File(imgClassDirectory + "/negatives/").listFiles()) {
			if (file.getName().endsWith(".png")) {
				Mat img = Imgcodecs.imread(file.getPath());
				Imgproc.cvtColor(img, img, Imgproc.COLOR_BGR2GRAY);
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
		System.out.println(trainingData.rows());
		System.out.println(classes.rows());
		clasificador.train(trainingData, Ml.ROW_SAMPLE, trainingLabels);

		for (File file : new File(imgClassDirectory + "/samples/").listFiles()) {
			if (file.getName().endsWith(".png")) {
				Mat img = Imgcodecs.imread(file.getPath());
				Imgproc.cvtColor(img, img, Imgproc.COLOR_BGR2GRAY);
				Imgproc.resize(img, img, size);
				img = img.reshape(1, 1);
				img.convertTo(img, CvType.CV_32FC1);
				System.out.println(file.getName() + " : " + clasificador.predict(img));
			}
		}
	}

	// static int test2()
	// {
	// // Data for visual representation
	// int width = 512, height = 512;
	// Mat image = Mat::zeros(height, width, CV_8UC3);
	//
	// // Set up training data
	// int labels[] = {1, -1, -1, -1};
	// Mat labelsMat(4, 1, CV_32SC1, labels);
	//
	// float trainingData[][] = { {501, 10}, {255, 10}, {501, 255}, {10, 501} };
	// Mat trainingDataMat(4, 2, CV_32FC1, trainingData);
	//
	// // Set up SVM's parameters
	// Ptr svm = ml::SVM::create();
	// svm->setType(ml::SVM::C_SVC);
	// svm->setKernel(ml::SVM::LINEAR);
	// svm->setTermCriteria(cv::TermCriteria(CV_TERMCRIT_ITER, 100, 1e-6));
	//
	// // Train the SVM
	// svm->train(trainingDataMat, ml::ROW_SAMPLE, labelsMat);
	//
	// Vec3b green(0,255,0), blue (255,0,0);
	// // Show the decision regions given by the SVM
	// for (int i = 0; i < image.rows; ++i)
	// for (int j = 0; j < image.cols; ++j)
	// {
	// Mat sampleMat = (Mat_(1,2) << j,i); float response = svm->predict(sampleMat);
	//
	// if (response == 1)
	// image.at(i,j) = green;
	// else if (response == -1)
	// image.at(i,j) = blue;
	// }
	//
	// // Show the training data
	// int thickness = -1;
	// int lineType = 8;
	// circle( image, Point(501, 10), 5, Scalar( 0, 0, 0), thickness, lineType);
	// circle( image, Point(255, 10), 5, Scalar(255, 255, 255), thickness, lineType);
	// circle( image, Point(501, 255), 5, Scalar(255, 255, 255), thickness, lineType);
	// circle( image, Point( 10, 501), 5, Scalar(255, 255, 255), thickness, lineType);
	//
	// // Show support vectors
	// thickness = 2;
	// lineType = 8;
	//
	// Mat supVecs = svm->getSupportVectors();
	// int c = supVecs.rows;
	//
	// for (int i = 0; i < c; ++i)
	// {
	// std::vector v;
	// v.assign(supVecs.row(i).datastart, supVecs.row(i).dataend);
	// circle( image, Point( (int) v[0], (int) v[1]), 6, Scalar(128, 128, 128), thickness, lineType);
	// }
	//
	// imwrite("result.png", image); // save the image
	//
	// imshow("SVM Simple Example", image); // show it to the user
	// waitKey(0);
	//
	// }
}
