package org.genericsystem.layout;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

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
		Img img = new Img("resources/14342661748973931.jpg");
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		Img binary = img.bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, 17, 15).cleanTables();
		Layout layout = img.buildLayout(new Size(0.04, 0.008), 8, 0.008f, binary);
		layout.draw(img, new Scalar(0, 255, 0), 1);
		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		mainGrid.add(new ImageView(binary.toJfxImage()), columnIndex, rowIndex++);

		Img total = new Img("resources/14342661748973931.jpg");
		layout.ocrTree(total, 2);
		System.out.println(layout.recursivToString());
		System.out.println("################################################################");
		// /layout.ocrTree(total, 4);

	}
}
