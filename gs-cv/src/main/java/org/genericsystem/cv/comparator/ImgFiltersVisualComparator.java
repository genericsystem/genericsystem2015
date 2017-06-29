package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.ImgClass;
import org.genericsystem.cv.Tools;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.ZoneScorer;
import org.genericsystem.cv.Zones;
import org.opencv.core.Core;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

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
		
		Img img = Tools.firstImg(imgClassDirectory);
		List<Integer> blockSizes = Arrays.asList(new Integer[]{3,5,7,9,11,15,17,21,27,37});
		List<Double> ks = Arrays.asList(new Double[]{-2.0,-1.0,-0.8,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,
													0.0,
													0.1,0.2,0.3,0.4,0.5,0.6,0.8,1.0,2.0,3.0});
		for (Integer bs : blockSizes){
			for (Double k : ks){
				Img img2 = img.niblackThreshold(bs, k); // k: between -1.0 and 0.0, bs > 5
//				Img img2 = img.sauvolaThreshold(bs, k); // k: between 0.1 and 0.3, bs > 5
//				Img img2 = img.nickThreshold(bs, k); // k: between -0.3 and -0.1, bs > 7
//				Img img2 = img.wolfThreshold(bs, k); // k: between 0.1 and 0.6, bs > 5
//				Img img2 = img.adaptativeMeanThreshold(bs, k);
//				Img img2 = img.adaptativeGaussianThreshold(bs, k);
				String text = "bs=" + bs + ", k=" + k;
				Imgproc.putText(img2.getSrc(), text, new Point(550, 129), Core.FONT_HERSHEY_PLAIN, 3, new Scalar(0, 0, 255), 3);
				mainGrid.add(img2.getImageView(), columnIndex++, rowIndex);
			}
			rowIndex++;
			columnIndex = 0;
		}

//		Img img2 = Tools.firstImg(imgClassDirectory);
//		mainGrid.add(img2.niblackThreshold(15, 0).getImageView(), columnIndex, rowIndex++);
//		Img img3 = Tools.firstImg(imgClassDirectory);
//		mainGrid.add(img3.niblackThreshold(15, -1).getImageView(), columnIndex, rowIndex++);
//		Img img31 = Tools.firstImg(imgClassDirectory);
//		mainGrid.add(img31.niblackThreshold(15, -0.75).getImageView(), columnIndex, rowIndex++);
//		Img img32 = Tools.firstImg(imgClassDirectory);
//		mainGrid.add(img32.niblackThreshold(15, -0.5).getImageView(), columnIndex, rowIndex++);

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
