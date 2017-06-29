package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.stream.Stream;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.ImgClass;
import org.genericsystem.cv.Tools;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.ZoneScorer;
import org.genericsystem.cv.Zones;
import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;

import javafx.scene.layout.GridPane;

public class ImgFiltersVisualComparator extends AbstractApp {
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
//		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
//		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);
//
//		mainGrid.add(imgClass.getMean().getImageView(), columnIndex, rowIndex++);
//		mainGrid.add(imgClass.getVariance().getImageView(), columnIndex, rowIndex++);

//		Img img1 = Tools.firstImg(imgClassDirectory);
//		mainGrid.add(img1.otsu().getImageView(), columnIndex, rowIndex++);
//		
//		Img img11 = Tools.firstImg(imgClassDirectory);
//		mainGrid.add(img11.otsuAfterGaussianBlur(new Size(5, 5)).getImageView(), columnIndex, rowIndex++);

		Img img2 = Tools.firstImg(imgClassDirectory);
		mainGrid.add(img2.niblackThreshold(15, 0).getImageView(), columnIndex, rowIndex++);
		Img img3 = Tools.firstImg(imgClassDirectory);
		mainGrid.add(img3.niblackThreshold(15, -1).getImageView(), columnIndex, rowIndex++);
		Img img31 = Tools.firstImg(imgClassDirectory);
		mainGrid.add(img31.niblackThreshold(15, -0.75).getImageView(), columnIndex, rowIndex++);
		Img img32 = Tools.firstImg(imgClassDirectory);
		mainGrid.add(img32.niblackThreshold(15, -0.5).getImageView(), columnIndex, rowIndex++);

//		Zones zones;
//		try {
//			zones = Zones.load(imgClassDirectory);
//		} catch (RuntimeException e) {
//			System.out.println("could not load accurate zones");
//			imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(20, 3)));
//			zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
//		}
//		Img model = imgClass.getMean();
//		zones.draw(model, new Scalar(0, 255, 0), 3);
//		mainGrid.add(model.getImageView(), columnIndex, rowIndex++);
//		int i = 0;
//		for (File file : new File(imgClassDirectory).listFiles())
//			if (file.getName().endsWith(".png")) {
//				System.out.println("File : " + file.getName());
//				if (i++ > 3)
//					continue;
//				Img img = new Img(Imgcodecs.imread(file.getPath()));
//				for (Zone zone : zones) {
//					System.out.println("Zone n°" + zone.getNum());
//					zone.draw(img, new Scalar(0, 255, 0), -1);
//					ZoneScorer scorer = zone.newUnsupervisedScorer(Stream.concat(
//							Tools.classImgsStream(imgClassDirectory, file.getName()),
//							Tools.classImgsStream(imgClassDirectory + "/mask/" + file.getName().replace(".png", ""))));
//					// // zone.write(img,
//					// // scorer.getBestText() + " " +
//					// // Math.floor((scorer.getBestScore() * 10000)) / 100 +
//					// "%",
//					// // 2.5,
//					// // new Scalar(0, 0, 255), 2);
//					zone.write(img, scorer.getBestText2(), 2.5, new Scalar(0, 0, 255), 2);
//					System.out.println("Best text : " + scorer.getBestText2());
//				}
//				mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
//			}
	}
}
