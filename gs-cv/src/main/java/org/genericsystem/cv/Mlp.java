package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfFloat;
import org.opencv.core.MatOfInt;
import org.opencv.core.TermCriteria;
import org.opencv.ml.ANN_MLP;
import org.opencv.ml.Ml;

public class Mlp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}
	final int MAX_DATA = 1000;
	ANN_MLP mlp;
	int input;
	int output;
	ArrayList<float[]> train;
	ArrayList<float[]> label;
	MatOfFloat result;

	public static void main(String[] args) {
		Mlp mlp = new Mlp(2, 2);

		mlp.addData(new float[] { 0, 0 }, new float[] { 1, 0 });
		mlp.addData(new float[] { 1, 1 }, new float[] { 0, 1 });
		mlp.addData(new float[] { 0, 1 }, new float[] { 1, 0 });
		mlp.addData(new float[] { 1, 0 }, new float[] { 1, 0 });

		mlp.train();

		System.out.println("0 xor 0, 0 or 0 = " + Arrays.toString(mlp.predict(new float[] { 0, 0 })));
		System.out.println("1 xor 1, 1 or 1 = " + Arrays.toString(mlp.predict(new float[] { 1, 1 })));
		System.out.println("0 xor 1, 0 or 1 = " + Arrays.toString(mlp.predict(new float[] { 0, 1 })));
		System.out.println("1 xor 0, 1 or 0 = " + Arrays.toString(mlp.predict(new float[] { 1, 0 })));
	}

	public Mlp(int input, int output) {
		this.input = input;
		this.output = output;
		mlp = ANN_MLP.create();
		MatOfInt m1 = new MatOfInt(input, 8, output);
		mlp.setLayerSizes(m1);
		mlp.setActivationFunction(ANN_MLP.SIGMOID_SYM);
		// mlp.setTrainMethod(ANN_MLP.BACKPROP);
		mlp.setTermCriteria(new TermCriteria(TermCriteria.MAX_ITER + TermCriteria.EPS, 100000, 0.00001f));
		result = new MatOfFloat();
		train = new ArrayList<>();
		label = new ArrayList<>();
	}

	void addData(float[] t, float[] l) {
		if (t.length != input)
			return;
		if (train.size() >= MAX_DATA)
			return;
		train.add(t);
		label.add(l);
	}

	int getCount() {
		return train.size();
	}

	void train() {
		float[][] tr = new float[train.size()][input];
		for (int i = 0; i < train.size(); i++) {
			for (int j = 0; j < train.get(i).length; j++) {
				tr[i][j] = train.get(i)[j];
			}
		}
		Mat response = new Mat(label.size(), label.get(0).length, CvType.CV_32FC1);
		for (int i = 0; i < label.size(); i++)
			for (int j = 0; j < label.get(0).length; j++)
				response.put(i, j, label.get(i)[j]);
		float[] trf = flatten(tr);
		Mat trainData = new Mat(train.size(), input, CvType.CV_32FC1);
		trainData.put(0, 0, trf);
		mlp.train(trainData, Ml.ROW_SAMPLE, response);
		trainData.release();
		response.release();
		train.clear();
		label.clear();
	}

	float[] predict(float[] i) {
		if (i.length != input)
			throw new IllegalStateException();
		Mat test = new Mat(1, input, CvType.CV_32FC1);
		test.put(0, 0, i);
		double val = mlp.predict(test, result, 0);
		// if (val != 0)
		// System.out.println(val);
		return getResult();
	}

	float[] getResult() {
		return result.toArray();
	}

	float[] flatten(float[][] a) {
		if (a.length == 0)
			return new float[] {};
		int rCnt = a.length;
		int cCnt = a[0].length;
		float[] res = new float[rCnt * cCnt];
		int idx = 0;
		for (int r = 0; r < rCnt; r++) {
			for (int c = 0; c < cCnt; c++) {
				res[idx] = a[r][c];
				idx++;
			}
		}
		return res;
	}
}
