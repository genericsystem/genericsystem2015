package org.genericsystem.layout;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

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

		Img adaptivSplit = img.bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY, 17, 15);

		Layout layout = img.buildLayout(new Size(0.036, 0.008), 5, 0.008f, img, adaptivSplit);
		// layout.draw(layout.getRoi(img), new Scalar(0, 255, 0), 1);
		// mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
		// mainGrid.add(new ImageView(adaptivSplit.toJfxImage()), columnIndex, rowIndex++);
		//
		Img ultimate = new Img("resources/14342661748973931.jpg").cleanTables();

		// mainGrid.add(new ImageView(ultimate.toJfxImage()), columnIndex, rowIndex++);
		// img = new Img("resources/14342661748973931.jpg");
		// layout = img.buildLayout(new Size(0.036, 0.008), 7, 0.008f, img, ultimate);
		// layout.draw(layout.getRoi(img), new Scalar(0, 255, 0), 1);
		// mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);

		// System.out.println(layout.recursivToString());
		Img total = new Img("resources/14342661748973931.jpg");
		System.out.println(layout);
		layout.ocrTree(layout.getRoi(total), total);
		System.out.println(layout.recursivToString());

		// Layout layout = new Layout(0.5, 1, 0.5, 1);
		// Layout layout2 = new Layout(0.5, 1, 0.5, 1);
		// Layout parent = new Layout(0, 1, 0, 1);
		// layout.setParent(parent);
		// layout2.setParent(layout);
		// Rect rect = layout.getRect(img);
		// Rect rect2 = layout2.getRect(img);
		// System.out.println(img.rows() + "x" + img.cols());
		// System.out.println(rect.toString());
		// System.out.println(rect2.toString());

	}
}
