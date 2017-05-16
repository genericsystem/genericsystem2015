package org.genericsystem.cv;

import java.io.File;
import java.util.List;

import org.genericsystem.cv.ZoneScorer.UnsupervisedZoneScorer;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;

import javafx.scene.layout.GridPane;

public class ClassImgFieldsDetector extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "aligned-image-3.png";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;

		ImgClass imgClass = ImgClass.fromDirectory(null, classImgRepertory);
		Img model = imgClass.getMean();

		imgClass.addMapper(img -> img);
		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

		imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(15, 3)));
		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

		Zones zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
		zones.draw(model, new Scalar(0, 255, 0), 3);
		mainGrid.add(model.getImageView(), columnIndex, rowIndex++);

		for (File file : new File(classImgRepertory).listFiles())
			if (file.getName().endsWith(".png")) {
				System.out.println("file : " + file.getName());
				Img img = new Img(Imgcodecs.imread(file.getPath()));
				try {
					List<Mat> sameMats = Tools.getClassMats(classImgRepertory + "/mask/" + file.getName().replace(".png", ""));
					for (Zone zone : zones.get()) {
						zone.draw(img, new Scalar(0, 255, 0), -1);
						UnsupervisedZoneScorer scorer = zone.newUnsupervisedScorer(sameMats);
						zone.write(img, scorer.getBestText() + " " + Math.floor((scorer.getBestScore() * 10000)) / 100 + "%", 2.5, new Scalar(0, 0, 255), 2);
					}
					mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
				} catch (Exception ignore) {

				}
			}

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