package org.genericsystem.cv;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class LayoutAnalyzer extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;
		// mainGrid.add(new ImageView(new Img("resources/14342661748973931.jpg").toJfxImage()), columnIndex, rowIndex++);

		rowIndex = 0;
		columnIndex++;
		Img img = new Img("resources/14342661748973931.jpg");
		Img adaptivSplit = img.bgr2Gray();
		adaptivSplit.adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, 17, 15);
		adaptivSplit.recursivSplit(new Size(0.024, 0.009), 100, 0.01f, img, (roi, zones) -> zones.draw(roi, new Scalar(0, 255, 0), 1));
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		mainGrid.add(new ImageView(adaptivSplit.toJfxImage()), columnIndex, rowIndex++);

		// img = new Img("resources/14342661748973931.jpg");
		// Img otsuSplit = img.bgr2Gray().thresHold(0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
		// // .morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(1.1, 1.1)).morphologyEx(Imgproc.MORPH_OPEN, Imgproc.MORPH_RECT, new Size(1.1, 1.1));
		// otsuSplit.recursivSplit(0.012, 100, 0.01f, img, (roi, zones) -> zones.draw(roi, new Scalar(0, 255, 0), 1));
		// mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		// mainGrid.add(new ImageView(otsuSplit.toJfxImage()), columnIndex, rowIndex++);

	}
}
