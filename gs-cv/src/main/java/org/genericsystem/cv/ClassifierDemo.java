package org.genericsystem.cv;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Size;
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

public class ClassifierDemo extends Application {

	private final static String sourceDirectoryPath = "png";
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

		List<File> images = Arrays.stream(new File(sourceDirectoryPath).listFiles()).filter(img -> img.getName().endsWith(".png")).collect(Collectors.toList());

		int row = 0;

		for (File img1 : images) {
			int column = 0;
			File img2 = images.get(0);
			Mat mat1 = Imgcodecs.imread(img1.getPath());
			gridPane.add(getImageViewFromMat(mat1), column++, row);
			Integer result = Classifier.compareFeature(img1.getPath(), img2.getPath());
			gridPane.add(new Label(result.toString()), column++, row);
			row++;
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
