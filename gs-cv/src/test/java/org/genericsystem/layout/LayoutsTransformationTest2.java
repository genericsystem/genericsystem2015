package org.genericsystem.layout;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class LayoutsTransformationTest2 extends AbstractApp {

	static {
		NativeLibraryLoader.load();
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

		Img binary1 = new Img(new Mat(new Size(800, 600), CvType.CV_8UC3, new Scalar(255, 255, 255)), false);
		new Zone(0, new Rect(new Point(200, 200), new Point(300, 300))).draw(binary1, new Scalar(0, 0, 0), -1);
		new Zone(1, new Rect(new Point(400, 400), new Point(500, 500))).draw(binary1, new Scalar(0, 0, 0), -1);

		Layout layout1 = binary1.adaptativeGaussianThreshold(17, 15).buildLayout(new Size(0.04, 0.008), 8);
		layout1.draw(binary1, new Scalar(0, 255, 0), 2);
		mainGrid.add(new ImageView(binary1.toJfxImage()), columnIndex, rowIndex++);
		System.out.println(layout1.recursiveToString());

		Img binary2 = new Img(new Mat(new Size(800, 600), CvType.CV_8UC3, new Scalar(255, 255, 255)), false);
		new Zone(0, new Rect(new Point(250, 200), new Point(350, 300))).draw(binary2, new Scalar(0, 0, 0), -1);
		new Zone(1, new Rect(new Point(450, 400), new Point(550, 500))).draw(binary2, new Scalar(0, 0, 0), -1);

		Layout layout2 = binary2.adaptativeGaussianThreshold(17, 15).buildLayout(new Size(0.04, 0.008), 8);
		layout2.draw(binary2, new Scalar(0, 255, 0), 2);
		mainGrid.add(new ImageView(binary2.toJfxImage()), columnIndex, rowIndex++);
		System.out.println(layout2.recursiveToString());

		Img binary3 = new Img(new Mat(new Size(160, 120), CvType.CV_8UC3, new Scalar(255, 255, 255)), false);
		new Zone(0, new Rect(new Point(40, 40), new Point(60, 60))).draw(binary3, new Scalar(0, 0, 0), -1);
		new Zone(1, new Rect(new Point(80, 80), new Point(100, 100))).draw(binary3, new Scalar(0, 0, 0), -1);

		Layout layout3 = binary3.adaptativeGaussianThreshold(17, 15).buildLayout(new Size(0.04, 0.008), 8);
		layout3.draw(binary3, new Scalar(0, 255, 0), 2);
		mainGrid.add(new ImageView(binary3.toJfxImage()), columnIndex, rowIndex++);
		System.out.println(layout3.recursiveToString());

		// Close the images
		binary1.close();
		binary2.close();
		binary3.close();
	}
}
