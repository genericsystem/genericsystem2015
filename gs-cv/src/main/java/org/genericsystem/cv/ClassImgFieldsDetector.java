package org.genericsystem.cv;

import java.io.File;
import java.util.stream.Stream;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;

import javafx.scene.layout.GridPane;

public class ClassImgFieldsDetector extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String imgClassDirectory = "classes/id-fr-front";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int columnIndex = 0;
		int rowIndex = 0;

		ImgClass imgClass = ImgClass.fromDirectory(imgClassDirectory);
		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

		// mainGrid.add(imgClass.getMean().getImageView(), columnIndex,
		// rowIndex++);
		// mainGrid.add(imgClass.getVariance().getImageView(), columnIndex,
		// rowIndex++);
		//
		// Img img1 = Tools.firstImg(imgClassDirectory);
		// Zones.get(img1.sobel(), 300).draw(img1, new Scalar(0, 255, 0), 3);
		// mainGrid.add(img1.getImageView(), columnIndex, rowIndex++);
		//
		// Img img2 = Tools.firstImg(imgClassDirectory);
		// Zones.get(img2.mser(), 300).draw(img2, new Scalar(255, 0, 0), 3);
		// mainGrid.add(img2.getImageView(), columnIndex, rowIndex++);
		//
		// Img img3 = Tools.firstImg(imgClassDirectory);
		// Zones.get(img3.grad(), 300).draw(img3, new Scalar(0, 0, 255), 3);
		// mainGrid.add(img3.getImageView(), columnIndex, rowIndex++);

		Zones zones;
		try {
			zones = Zones.load(imgClassDirectory);
		} catch (RuntimeException e) {
			System.out.println("could not load accurate zones");
			imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(20, 3)));
			zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
		}
		Img model = imgClass.getMean();
		zones.draw(model, new Scalar(0, 255, 0), 3);
		mainGrid.add(model.getImageView(), columnIndex, rowIndex++);
		int i = 0;
		for (File file : new File(imgClassDirectory).listFiles())
			if (file.getName().endsWith(".png")) {
				System.out.println("File : " + file.getName());
				if (i++ > 3)
					continue;
				Img img = new Img(Imgcodecs.imread(file.getPath()));
				for (Zone zone : zones) {
					System.out.println("Zone n°" + zone.getNum());
					zone.draw(img, new Scalar(0, 255, 0), -1);
					ZoneScorer scorer = zone.newUnsupervisedScorer(Stream.concat(Tools.classImgsStream(imgClassDirectory, file.getName()), Tools.classImgsStream(imgClassDirectory + "/mask/" + file.getName().replace(".png", ""))));
					// // zone.write(img,
					// // scorer.getBestText() + " " +
					// // Math.floor((scorer.getBestScore() * 10000)) / 100 +
					// "%",
					// // 2.5,
					// // new Scalar(0, 0, 255), 2);
					zone.write(img, scorer.getBestText2(), 2.5, new Scalar(0, 0, 255), 2);
					System.out.println("Best text : " + scorer.getBestText2());
				}
				mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
			}
	}
}
