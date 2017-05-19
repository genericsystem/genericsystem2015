package org.genericsystem.cv;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Size;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.FeatureDetector;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class ClassifierParametersTest extends Application {

	private final static String pngDirectoryPath = "png";
	private final static String adjustedDirectoryPath = "adjusted";

	private final double displayWidth = 200d;

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void start(Stage stage) throws Exception {

		GridPane gridPane = new GridPane();

		List<File> pngImgs = Arrays.stream(new File(pngDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).collect(Collectors.toList());
		List<File> adjustedImages = Arrays.stream(new File(adjustedDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).collect(Collectors.toList());

		int row = 0;

		// List<List<File>> classifiedFiles = partition(pngImgs, (file1, file2) -> Classifier.compareFeature(file1.getPath(), file2.getPath()) != null);
		// for (List<File> typeList : classifiedFiles) {
		// gridPane.add(new Separator(), 0, row);
		// row++;
		// int column = 0;
		// for (File img : typeList) {
		// Mat mat = Imgcodecs.imread(img.getPath());
		// gridPane.add(getImageViewFromMat(mat), column++, row);
		// }
		// row++;
		// }

		for (File img1 : pngImgs) {
			System.out.println("================== image : " + img1.getName());
			int column = 0;
			Mat mat1 = Imgcodecs.imread(img1.getPath());
			gridPane.add(getImageViewFromMat(mat1), column++, row);
			for (File img2 : adjustedImages) {
				Mat mat2 = Imgcodecs.imread(img2.getPath());
				gridPane.add(getImageViewFromMat(mat2), column++, row);
				Mat result = Classifier.compareFeature(img1.getPath(), img2.getPath(), 50, FeatureDetector.GFTT, DescriptorExtractor.OPPONENT_ORB);
				if (result != null)
					gridPane.add(getImageViewFromMat(result), column++, row);
				else
					gridPane.add(new Label("Not matching"), column++, row);
			}
			row++;
			System.gc();
			System.runFinalization();
		}
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Computer Vision");
		ScrollPane scrollPane = new ScrollPane(gridPane);
		scrollPane.setFitToHeight(true);
		VBox root = new VBox(scrollPane);
		scene.setRoot(root);
		stage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent event) {
				try {
					stop();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		});
		stage.setScene(scene);
		stage.show();
	}

	private <T> List<List<T>> partition(List<T> elements, BiFunction<T, T, Boolean> equivalent) {
		List<List<T>> results = new ArrayList<>();
		elements.forEach(element -> {
			for (List<T> testList : results) {
				if (equivalent.apply(element, testList.get(0))) {
					testList.add(element);
					return;
				}
			}
			List<T> newList = new ArrayList<>();
			newList.add(element);
			results.add(newList);
		});
		return results;
	}

	private ImageView getImageViewFromMat(Mat src) {
		Mat target = new Mat();
		Imgproc.resize(src, target, new Size(displayWidth, Math.floor((displayWidth / src.width()) * src.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(displayWidth);
		return imageView;
	}
}
