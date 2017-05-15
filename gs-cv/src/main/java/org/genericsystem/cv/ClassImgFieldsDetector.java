package org.genericsystem.cv;

import javafx.scene.layout.GridPane;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

public class ClassImgFieldsDetector extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "aligned-image-3.png";

	// private final static String adjustedDirectoryPath2 = "aligned-image-3.png/mask/image-3";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;

		ImgClass imgClass = ImgClass.fromDirectory(null, classImgRepertory);
		Img model = imgClass.getMean();
		ImgClass imgClass2 = ImgClass.fromDirectory(null, classImgRepertory);
		Img model2 = imgClass2.getMean();

		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

		imgClass.addMapper(img -> img.dilateBlacks(86, 255, 76, new Size(15, 3)));
		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

		imgClass2.addMapper(img -> img.dilateBlacks(86, 255, 76, new Size(15, 3)));
		mainGrid.add(imgClass2.getMean().getImageView(), columnIndex + 1, rowIndex - 2);
		mainGrid.add(imgClass2.getVariance().getImageView(), columnIndex + 1, rowIndex - 1);

		Zones zones = imgClass.getClosedMeanZones(300, 6, 6, new Size(9, 10));
		zones.draw(model, new Scalar(0, 255, 0), 3);
		mainGrid.add(model.getImageView(), columnIndex, rowIndex++);

		Img img = new Img(Tools.classImgsStream(classImgRepertory).iterator().next(), zones.get().get(0));
		Zones subZones = Zones.get(img/* range(new Scalar(0, 0, 0), new Scalar(255, 255, 180), true) */.otsuInv(), 1, 6, 15);
		subZones.draw(img, new Scalar(0, 0, 255), 2);
		mainGrid.add(img.getImageView(), columnIndex + 1, rowIndex - 1);

		Img img2 = new Img(Tools.classImgsStream(classImgRepertory).iterator().next(), zones.get().get(0));
		for (Zone z : subZones.get()) {
			Img letter = new Img(img2, z).resize(0.9);
			System.out.println(Ocr.doWork(letter.getSrc()));
			mainGrid.add(letter.getImageView(), columnIndex + 1, rowIndex++);
		}

		// List<Mat> sameMats = Tools.getClassMats("aligned-image-3.png/mask/image-3");
		// // sameMats.addAll(Tools.getImages("aligned-image-3.png", "image-3.png"));
		// Img img4 = Tools.classImgsStream(classImgRepertory).iterator().next();
		//
		// int i = 0;
		// for (Zone zone : zones.get()) {
		// zone.draw(img4, new Scalar(0, 255, 0), -1);
		// zone.write(img4, "z" + i++ + ":" + Math.floor((zone.computeUnsupervisedScore(sameMats) * 10000)) / 100 + "%", 2, new Scalar(0, 0, 255), 2);
		// }
		// mainGrid.add(img4.getImageView(), columnIndex, rowIndex++);
		//
		// Img img5 = Tools.classImgsStream(classImgRepertory).iterator().next();
		// i = 0;
		// for (Zone zone : zones2.get()) {
		// zone.draw(img5, new Scalar(0, 255, 0), -1);
		// zone.write(img5, "z" + i++ + ":" + Math.floor((zone.computeUnsupervisedScore(sameMats) * 10000)) / 100 + "%", 2, new Scalar(0, 0, 255), 2);
		// }
		// mainGrid.add(img5.getImageView(), columnIndex + 1, rowIndex - 1);

	}
	// Img img = Tools.classImgsStream(classImgRepertory).iterator().next();
	// ImgZoner.drawZones(img.sobel(), img, 300, new Scalar(0, 255, 0), 3);
	// mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
	//
	// Img img2 = Tools.classImgsStream(classImgRepertory).iterator().next();
	// ImgZoner.drawZones(img2.mser(), img2, 300, new Scalar(0, 255, 0), 3);
	// mainGrid.add(img2.getImageView(), columnIndex, rowIndex++);
	//
	// Img img3 = Tools.classImgsStream(classImgRepertory).iterator().next();
	// ImgZoner.drawZones(img3.grad(), img3, 300, new Scalar(0, 255, 0), 3);
	// mainGrid.add(img3.getImageView(), columnIndex, rowIndex++);
}