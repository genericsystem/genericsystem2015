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
		// mainGrid.add(img.gray().sobel(CvType.CV_8UC1, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT).getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(img.gray().sobel(CvType.CV_8UC1, 1, 0, 3, 1, 0, Core.BORDER_DEFAULT).morphologyEx(type, new StructuringElement(Imgproc.MORPH_RECT, size)).getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(img.canny(40, 80).getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(img.canny(40, 80).morphologyEx(type, new StructuringElement(Imgproc.MORPH_RECT, size)).getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(img.gray().adaptiveThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 11, 12).getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(img.gray().adaptiveThresHold(255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 11, 12).morphologyEx(type, new StructuringElement(Imgproc.MORPH_RECT, size)).getImageView(), columnIndex, rowIndex++);

		mainGrid.add(img.otsuInv().getImageView(), columnIndex, rowIndex++);
		Img hImg = img.otsuInv().morphologyEx(type, new StructuringElement(Imgproc.MORPH_RECT, hSize));
		Img vImg = img.otsuInv().morphologyEx(type, new StructuringElement(Imgproc.MORPH_RECT, vSize));

		mainGrid.add(hImg.add(vImg).getImageView(), columnIndex, rowIndex++);
		mainGrid.add(hImg.bitwise(vImg).getImageView(), columnIndex, rowIndex++);
		Zones zones = Zones.get(hImg.add(vImg), 10, Imgproc.RETR_TREE);
		Img result3 = hImg.bitwise(vImg);
		zones.draw(result3, new Scalar(255), 5);
		mainGrid.add(result3.getImageView(), columnIndex, rowIndex++);
		for (Zone zone : zones.get())
			mainGrid.add(zone.getRoi(img.otsuInv()).getImageView(), columnIndex, rowIndex++);

		mainGrid.add(img.otsuInv().projectVerticaly().toVerticalHistogram(img.cols()).getImageView(), columnIndex, rowIndex++);

		mainGrid.add(img.otsuInv().projectHorizontaly().toHorizontalHistogram(img.rows()).getImageView(), columnIndex, rowIndex++);

	}
}
