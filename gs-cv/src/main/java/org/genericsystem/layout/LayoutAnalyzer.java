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
		Img img = new Img("classes/id-fr-front/image-4.png");
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		Img binary = img.cleanFaces().bilateralFilter().adaptativeGaussianThreshold(17, 15).cleanTables();
		Layout layout = img.buildLayout(new Size(0.04, 0.008), 8, binary);
		layout.draw(img, new Scalar(0, 255, 0), 1);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		mainGrid.add(new ImageView(binary.toJfxImage()), columnIndex, rowIndex++);

		Img total = new Img("classes/id-fr-front/image-4.png");
		layout.ocrTree(total, 10);
		mainGrid.add(new ImageView(total.toJfxImage()), columnIndex, rowIndex++);
		System.out.println(layout.recursivToString());

	}
}
