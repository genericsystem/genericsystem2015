package org.genericsystem.cv;

import java.util.Arrays;
import java.util.List;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.core.TermCriteria;
import org.opencv.ml.Boost;
import org.opencv.ml.Ml;
import org.opencv.ml.SVM;
import org.opencv.utils.Converters;

public class Booster {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {

		List<Double> labels = Arrays.asList(1.0, 1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0);
		List<double[]> trainingData = Arrays.asList(new double[] { 501, 10 }, new double[] { 508, 15 }, new double[] { 255, 10 }, new double[] { 501, 255 }, new double[] { 10, 501 }, new double[] { 10, 501 }, new double[] { 11, 501 },
				new double[] { 9, 501 }, new double[] { 10, 502 }, new double[] { 10, 511 }, new double[] { 10, 495 });

		// Mat labelsMat = Converters.vector_float_to_Mat(labels);
		Mat labelsMat = new Mat(11, 1, CvType.CV_32S);
		for (int i = 0; i < 11; i++)
			labelsMat.push_back(new Mat(new Size(1, 1), CvType.CV_32S, new Scalar(labels.get(i))));

		Mat trainingDataMat = new Mat(11, 2, CvType.CV_32FC1);
		for (int i = 0; i < 11; i++) {
			trainingDataMat.put(i, 0, trainingData.get(i)[0]);
			trainingDataMat.put(i, 1, trainingData.get(i)[1]);
		}

		SVM svm = SVM.create();
		svm.setType(SVM.C_SVC);
		svm.setTermCriteria(new TermCriteria(TermCriteria.MAX_ITER, 100, 1e-6));
		svm.setKernel(SVM.LINEAR);
		svm.train(trainingDataMat, Ml.ROW_SAMPLE, labelsMat);

		Boost boost = Boost.create();
		boost.train(trainingDataMat, Ml.ROW_SAMPLE, labelsMat);

		// Test the classifiers
		Mat testSample1 = Converters.vector_double_to_Mat(Arrays.asList(251.0, 5.0));
		Mat testSample2 = Converters.vector_double_to_Mat(Arrays.asList(502.0, 11.0));

		float svmResponse1 = svm.predict(testSample1);
		float svmResponse2 = svm.predict(testSample2);

		float boostResponse1 = boost.predict(testSample1);
		float boostResponse2 = boost.predict(testSample2);

		System.out.println("SVM: " + svmResponse1 + " " + svmResponse2);
		System.out.println("BOOST: " + boostResponse1 + " " + boostResponse2);

	}
}

// Output:
// > SVM: -1 1
// > BOOST: -1 1
