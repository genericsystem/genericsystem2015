package org.genericsystem.cv;

import java.io.ByteArrayInputStream;

import org.genericsystem.cv.utils.NativeLibraryLoader;
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
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public abstract class AbstractApp extends Application {
	static {
		NativeLibraryLoader.load();
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

		scene.setOnKeyPressed(event -> {
			if (event.getCode() == KeyCode.SPACE)
				onSpace();
			if (event.getCode() == KeyCode.R)
				onR();
			if (event.getCode() == KeyCode.T)
				onT();
		});
		stage.setScene(scene);
		stage.show();
	}

	// hook
	protected void onSpace() {
		System.out.println("space pressed");
	}

	protected void onR() {
		System.out.println("r pressed");
	}
	
	protected void onT() {
		System.out.println("t pressed");
	}

	protected abstract void fillGrid(GridPane mainGrid);

	protected ImageView buildImageViewFromMat(Mat src) {
		Mat conv = new Mat();
		src.convertTo(conv, CvType.CV_8UC1);
		Mat target = new Mat();
		Imgproc.resize(conv, target, new Size(displayWidth, Math.floor((displayWidth / conv.width()) * conv.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(displayWidth);
		conv.release();
		target.release();
		buffer.release();
		return imageView;
	}

}
