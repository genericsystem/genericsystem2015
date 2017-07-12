package org.genericsystem.cv;

import org.opencv.core.Core;

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
		mainGrid.add(new ImageView(new Img("resources/14342661748973931.jpg").toJfxImage()), columnIndex, rowIndex++);
		mainGrid.add(new ImageView(new Img("resources/14342661748973931.jpg").recursivSplit(0.02, 3, 0.05).toJfxImage()), columnIndex, rowIndex++);
	}
}
