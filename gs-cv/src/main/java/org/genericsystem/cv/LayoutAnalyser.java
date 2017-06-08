package org.genericsystem.cv;

import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

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

		// mainGrid.add(hImg.add(vImg).getImageView(), columnIndex, rowIndex++);
		// mainGrid.add(hImg.bitwise(vImg).getImageView(), columnIndex, rowIndex++);
		Zones zones = Zones.get(hImg.add(vImg), 10, Imgproc.RETR_TREE);
		Img result3 = hImg.bitwise(vImg);
		zones.draw(result3, new Scalar(255), 5);
		mainGrid.add(result3.getImageView(), columnIndex, rowIndex++);

		recursivSplit(img, true);

		// for (Zone vZone : Zones.split(img, 30, 30, true).get()) {
		// Img roi = vZone.getRoi(img);
		// for (Zone hZone : Zones.split(roi, 20, 20, false).get()) {
		// Img subRoi = hZone.getRoi(roi);
		// for (Zone vZone2 : Zones.split(subRoi, 10, 10, true).get()) {
		// Img subSubRoi = vZone2.getRoi(subRoi);
		// // for
		// vZone2.draw(subRoi, new Scalar(255, 0, 0), 3);
		// }
		// hZone.draw(roi, new Scalar(0, 0, 255), 3);
		// }
		// vZone.draw(img, new Scalar(0, 255, 0), 3);
		// }
		mainGrid.add(img.getImageView(), columnIndex, rowIndex++);

	}

	void recursivSplit(Img roi, boolean vertical) {
		Zones zones = Zones.split(roi, 0, 0, vertical);
		assert zones.size() != 0;
		if (zones.size() == 1)
			return;
		for (Zone zone : zones) {
			Img subRoi = zone.getRoi(roi);
			recursivSplit(subRoi, !vertical);
		}
		zones.draw(roi, new Scalar(0, 255, 0), 3);
	}
}
