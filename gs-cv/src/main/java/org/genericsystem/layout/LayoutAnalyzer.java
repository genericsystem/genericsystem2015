package org.genericsystem.layout;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Scalar;

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
		// mainGrid.add(new ImageView(new Img("resources/14342661748973931.jpg").toJfxImage()), columnIndex, rowIndex++);
		rowIndex = 0;
		columnIndex++;
		Img img = new Img("resources/14342661748973931.jpg");
		Layout layout = img.buildLayout();
		layout.draw(img, new Scalar(0, 255, 0), 1);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		// mainGrid.add(new ImageView(adaptivSplit.toJfxImage()), columnIndex, rowIndex++);

		System.out.println(layout.recursivToString());
	}
}
