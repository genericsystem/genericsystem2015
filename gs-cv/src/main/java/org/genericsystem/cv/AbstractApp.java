package org.genericsystem.cv;

import java.io.ByteArrayInputStream;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public abstract class AbstractApp extends Application {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public final static double displayWidth = 400d;

	@Override
	public void start(Stage stage) throws Exception {
		GridPane gridPane = new GridPane();
		fillGrid(gridPane);
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Information Retriever");
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

	protected abstract void fillGrid(GridPane mainGrid);

	protected ImageView buildImageViewFromMat(Mat src) {
		Mat conv = new Mat();
		src.convertTo(conv, CvType.CV_8UC1);
		// System.out.println(conv);
		Mat target = new Mat();
		Imgproc.resize(conv, target, new Size(displayWidth, Math.floor((displayWidth / conv.width()) * conv.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(displayWidth);
		return imageView;
	}

}
