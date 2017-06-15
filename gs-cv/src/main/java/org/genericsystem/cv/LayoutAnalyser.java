package org.genericsystem.cv;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

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
		Size hSize = new Size(20, 1);
		Size vSize = new Size(1, 20);
		int type = Imgproc.MORPH_OPEN;
		Img img = new Img("resources/14342661748973931.jpg");
		mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
		mainGrid.add(img.otsuInv().getImageView(), columnIndex, rowIndex++);

		Img hImg = img.otsuInv().morphologyEx(type, Imgproc.MORPH_RECT, hSize);
		Img vImg = img.otsuInv().morphologyEx(type, Imgproc.MORPH_RECT, vSize);

		Zones zones = Zones.get(hImg.add(vImg), 10, Imgproc.RETR_TREE);
		Img result3 = hImg.bitwise(vImg);
		zones.draw(result3, new Scalar(255), 5);
		mainGrid.add(result3.getImageView(), columnIndex, rowIndex++);

		img.recursivSplit(30, true);
		mainGrid.add(img.getImageView(), columnIndex, rowIndex++);

	}

}
