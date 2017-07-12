package org.genericsystem.cv;

import java.util.Arrays;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class LayoutAnalyser extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	public static Mat close(Mat vector, double morph) {

		int k = new Double(Math.floor(morph * vector.rows())).intValue();
		double[] result = new double[vector.rows()];
		for (int i = 0; i < vector.rows(); i++) {
			System.out.println(vector.get(i, 0)[0]);
			if (i + 1 < vector.rows() && vector.get(i, 0)[0] == 255d && vector.get(i + 1, 0)[0] == 0) {
				for (int j = k + 1; j > 0; j--) {
					if (i + j < vector.rows()) {
						if (vector.get(i + j, 0)[0] == 255d) {
							Arrays.fill(result, i, i + j + 1, 255d);
							i += j - 1;
							break;
						}
						result[i] = vector.get(i, 0)[0];
					}
				}
			} else
				result[i] = vector.get(i, 0)[0];
		}

		Mat mat = new Mat(new Size(1, result.length), CvType.CV_8U);
		for (int i = 0; i < result.length; i++)
			mat.put(i, 0, result[i]);
		return mat;
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {

		// int[] vector = new int[] { 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0 };

		int columnIndex = 0;
		int rowIndex = 0;

		Img img = new Img("resources/14342661748973931.jpg").bgr2Gray().projectVertically();
		// img.recursivSplit(morph, level, percentage);

		Mat mask = new Mat();
		Core.inRange(img.getSrc(), new Scalar(25), new Scalar(220), mask);

		Img maskImg = new Img(mask);
		maskImg = maskImg.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(1, 8));

		Mat closed = close(mask, 8);

		// img.recursivSplit(0.020, 2, 0.0001);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(mask).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(maskImg.toJfxImage()), columnIndex++, rowIndex);

		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);
		mainGrid.add(new ImageView(new Img(closed).toJfxImage()), columnIndex++, rowIndex);

	}

}
