package org.genericsystem.layout;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

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
		rowIndex = 0;
		columnIndex++;
		// final String filename = "classes/id-fr-front/image-4.png";
		final String filename = "resources/14342661748973931.jpg";
		Img img = new Img(filename);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		Img binary = img.cleanFaces(0.1, 0.26).bilateralFilter(10, 80, 80).adaptativeGaussianThreshold(17, 15).cleanTables(0.05);
		Layout layout = binary.buildLayout(new Size(0.04, 0.008), 8);
		layout.draw(img, new Scalar(0, 255, 0), 1);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		mainGrid.add(new ImageView(binary.toJfxImage()), columnIndex, rowIndex++);

		Img img2 = new Img(filename).bilateralFilter(20, 80, 80);
		layout.ocrTree(img2, 6.0, 6.0);
		mainGrid.add(new ImageView(img2.toJfxImage()), columnIndex, rowIndex++);
		// mainGrid.add(new ImageView(binary2.toJfxImage()), columnIndex, rowIndex++);
		System.out.println(layout.recursiveToString());

		// Close the images
		img.close();
		binary.close();
		img2.close();
	}

}
