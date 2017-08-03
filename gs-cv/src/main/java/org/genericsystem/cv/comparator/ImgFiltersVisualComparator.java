package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
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

//		List<Integer> blockSizes = Arrays.asList(new Integer[] { 3, 5, 7, 9, 11, 15, 17, 21, 27, 37 });
//		List<Double> ks = Arrays.asList(new Double[] { -2.0, -1.0, -0.8, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0.0, 0.1,
//				0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 2.0, 3.0 });
//		for (Integer bs : blockSizes) {
//			for (Double k : ks) {
//				Img img2 = img.niblackThreshold(bs, k); // k: between -1.0 and
//														// 0.0, bs > 5
//				// Img img2 = img.sauvolaThreshold(bs, k); // k: between 0.1 and
//				// 0.3, bs > 5
//				// Img img2 = img.nickThreshold(bs, k); // k: between -0.3 and
//				// -0.1, bs > 7
//				// Img img2 = img.wolfThreshold(bs, k); // k: between 0.1 and
//				// 0.6, bs > 5
//				// Img img2 = img.adaptativeMeanThreshold(bs, k); // Img img2 =
//				img.adaptativeGaussianThreshold(bs, k);
//				String text = "bs=" + bs + ", k=" + k;
//				Imgproc.putText(img2.getSrc(), text, new Point(550, 129), Core.FONT_HERSHEY_PLAIN, 3,
//						new Scalar(0, 0, 255), 3);
//				mainGrid.add(img2.getImageView(), columnIndex++, rowIndex);
//			}
//			rowIndex++;
//			columnIndex = 0;
//		}

		final Map<String, ImgFunction> imgFilters = new HashMap<>();
		imgFilters.put("original", Img::bgr2Gray);
		imgFilters.put("niblack_37_m1.0", i -> i.niblackThreshold(37, -1.0));
		imgFilters.put("niblack_21_m1.0", i -> i.niblackThreshold(21, -1.0));
		imgFilters.put("niblack_21_m0.3", i -> i.niblackThreshold(21, -0.3));
		imgFilters.put("niblack_27_m1.8", i -> i.niblackThreshold(27, -0.8));
		imgFilters.put("niblack_7_m0.4", i -> i.niblackThreshold(7, -0.4));
//		imgFilters.put("bernsen", i -> i.bernsen(15, 15));
//		imgFilters.put("equalizeHisto", Img::equalizeHisto);
//		imgFilters.put("equalizeHistoAdaptative", i -> i.equalizeHistoAdaptative(4.0, new Size(8, 8)));
//		imgFilters.put("wolf", i -> i.wolfThreshold(15, 0.3));
//		imgFilters.put("nick", i -> i.nickThreshold(21, -0.1));
//		imgFilters.put("otsu", Img::otsu);
//		imgFilters.put("otsuGaussian", i -> i.otsuAfterGaussianBlur(new Size(3, 3)));
//		imgFilters.put("niblack", i -> i.niblackThreshold(15, -0.6));
//		imgFilters.put("sauvola", i -> i.sauvolaThreshold(15, 0.2));
//		imgFilters.put("adaptativeMeanThreshold", Img::adaptativeMeanThreshold);
//		imgFilters.put("adaptativeGaussianThreshold", Img::adaptativeGaussianThreshold);

		Map<String, Img> imgs = new HashMap<>();

		for (Entry<String, ImgFunction> entry : imgFilters.entrySet()) {
			System.out.print("Processing filter : " + entry.getKey() + "...");
			long start = System.currentTimeMillis();

			Img img2 = null;
			if ("original".equals(entry.getKey()) || "reality".equals(entry.getKey())) {
				img2 = img.bgr2Gray();
			} else {
				img2 = entry.getValue().apply(img);
			}
			imgs.put(entry.getKey(), img2);

			long stop = System.currentTimeMillis();
			System.out.println(" (" + (stop - start) + " ms)");
			// Imgproc.putText(img2.getSrc(), entry.getKey(), new Point(550,
			// 129), Core.FONT_HERSHEY_PLAIN, 5, new Scalar(0, 0, 255), 3);
			// mainGrid.add(img2.getImageView(), columnIndex, rowIndex++);
		}

		final Zones zones = Zones.load(imgClassDirectory);

		Img model = img;
		zones.draw(model, new Scalar(0, 255, 0), 3);
		mainGrid.add(model.getImageView(), columnIndex, rowIndex++);

		for (Entry<String, Img> entry : imgs.entrySet()) {
			Img currentImg = entry.getValue();
			for (Zone zone : zones) {
				// System.out.println("Zone n°" + zone.getNum());
				String ocr = zone.ocr(currentImg);
				zone.draw(currentImg, new Scalar(255, 255, 255), -1);
				zone.write(currentImg, ocr.trim(), 2, new Scalar(0, 0, 0), 3);

				Imgproc.rectangle(currentImg.getSrc(), new Point(80, 40), new Point(600, 90), new Scalar(255, 255, 255),
						-1);
				Imgproc.putText(currentImg.getSrc(), entry.getKey(), new Point(90, 80), Core.FONT_HERSHEY_PLAIN, 2.5,
						new Scalar(0, 0, 255), 3);
			}
			mainGrid.add(currentImg.getImageView(), columnIndex, rowIndex++);
			if (rowIndex % 3 == 0){
				rowIndex = 0;
				columnIndex++;
			}
		}
	}
}
