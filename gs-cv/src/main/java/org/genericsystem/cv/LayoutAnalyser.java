package org.genericsystem.cv;

import org.opencv.core.Core;
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

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;

		Img img = new Img("resources/14342661748973931.jpg");
		// mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(img.otsuInv().getImageView(), columnIndex, rowIndex++);

		Img hImg = img.otsuInv();
		hImg = hImg.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(1, 1 + 0.020 * hImg.height()));
		mainGrid.add(new ImageView(hImg.toJfxImage()), columnIndex, rowIndex++);

		hImg = hImg.projectVertically().toVerticalHistogram(hImg.cols(), 0.0001);
		mainGrid.add(new ImageView(hImg.toJfxImage()), columnIndex, rowIndex++);

		img.recursivSplit(0.020, 2, 0.0001);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);

	}

}
