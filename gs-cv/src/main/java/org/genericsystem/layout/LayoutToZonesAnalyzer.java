//package org.genericsystem.layout;
//
//import java.util.ArrayList;
//import java.util.List;
//
//import org.genericsystem.cv.AbstractApp;
//import org.genericsystem.cv.Img;
//import org.genericsystem.cv.Zones;
//import org.opencv.core.Scalar;
//import org.opencv.core.Size;
//
//import javafx.scene.image.ImageView;
//import javafx.scene.layout.GridPane;
//
//public class LayoutToZonesAnalyzer extends AbstractApp {
//
//	public static void main(String[] args) {
//		launch(args);
//	}
//
//	@Override
//	protected void fillGrid(GridPane mainGrid) {
//		int columnIndex = 0;
//		int rowIndex = 0;
//		rowIndex = 0;
//		columnIndex++;
//
//		// final String filename = "resources/14342661748973931.jpg";
//		final String filename = System.getenv("HOME") + "/genericsystem/gs-ir-files/other-pngs/201210-0085-0.png";
//		Img img = new Img(filename);
//		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
//		Img binary = img.cleanFaces(0.1, 0.26).bilateralFilter(20, 80, 80).adaptativeGaussianThreshold(17, 15).cleanTables(0.03);
//
//		// Layout layout = binary.buildLayout(new Size(0.04, 0.0068), 8);
//		Layout layout = binary.buildLayout(new Size(0.07, 0.008), 8);
//		layout.draw(img, new Scalar(0, 255, 0), 1);
//		mainGrid.add(new ImageView(img.toJfxImage()), columnIndex, rowIndex++);
//		mainGrid.add(new ImageView(binary.toJfxImage()), columnIndex, rowIndex++);
//
//		Img img2 = new Img(filename);
//		Zones zones = new Zones(img2, layout);
//		zones.draw(img2, new Scalar(0, 255, 0), 1);
//		mainGrid.add(new ImageView(img2.toJfxImage()), columnIndex, rowIndex++);
//
//		List<String> strings = new ArrayList<>();
//		zones.forEach(zone -> {
//			strings.add(zone.ocr(img2));
//		});
//		strings.forEach(System.out::println);
//
//		// --- Close images
//		img.close();
//		img2.close();
//	}
// }
