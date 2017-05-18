package org.genericsystem.cv;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.videoio.VideoCapture;

import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class VideoDisplay extends AbstractApp {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);

	}

	private VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer;
	private ImageView primaryFrame = new ImageView();
	private ImageView secondaryFrame = new ImageView();

	private Slider valueSlider, blueSlider, saturationSlider;

	@Override
	protected void fillGrid(GridPane mainGrid) {

		GridPane sliders = new GridPane();
		sliders.setPadding(new Insets(0, 40, 0, 40));

		Label valueLabel = new Label("Value");
		valueSlider = new Slider();
		valueSlider.setMin(0);
		valueSlider.setMax(255);
		valueSlider.setValue(40);
		sliders.add(valueLabel, 0, 0);
		sliders.add(valueSlider, 0, 1);

		Label blueLabel = new Label("Blue");
		blueLabel.setPadding(new Insets(20, 0, 0, 0));
		blueSlider = new Slider();
		blueSlider.setMin(0);
		blueSlider.setMax(255);
		blueSlider.setValue(40);
		sliders.add(blueLabel, 0, 2);
		sliders.add(blueSlider, 0, 3);

		Label saturationLabel = new Label("Saturation");
		saturationLabel.setPadding(new Insets(20, 0, 0, 0));
		saturationSlider = new Slider();
		saturationSlider.setMin(0);
		saturationSlider.setMax(255);
		saturationSlider.setValue(40);
		sliders.add(saturationLabel, 0, 4);
		sliders.add(saturationSlider, 0, 5);

		Mat frame = new Mat();
		this.capture.read(frame);
		Image imageToShow = Tools.mat2jfxImage(frame);
		primaryFrame.setImage(imageToShow);
		secondaryFrame.setImage(imageToShow);
		mainGrid.add(primaryFrame, 0, 0);
		mainGrid.add(secondaryFrame, 0, 1);
		mainGrid.add(sliders, 1, 1);

		Runnable frameGrabber = new Runnable() {

			@Override
			public void run() {
				// effectively grab and process a single frame
				Mat frame = new Mat();
				capture.read(frame);
				// convert and show the frame
				Image imageToShow = Tools.mat2jfxImage(frame);
				primaryFrame.setImage(imageToShow);
				Image imageToShow2 = Tools.mat2jfxImage(imageProcessing(frame));
				secondaryFrame.setImage(imageToShow2);
			}

		};

		this.timer = Executors.newSingleThreadScheduledExecutor();
		this.timer.scheduleAtFixedRate(frameGrabber, 0, 33, TimeUnit.MILLISECONDS);

	}

	private Mat imageProcessing(Mat frame) {

		// Imgproc.cvtColor(frame, frame, Imgproc.COLOR_BGR2GRAY);
		Img img = new Img(frame);
		int value = (int) Math.round(valueSlider.getValue());
		int blue = (int) Math.round(blueSlider.getValue());
		int saturation = (int) Math.round(saturationSlider.getValue());
		Img img2 = img.dilateBlacks(value, blue, saturation, new Size(15, 3));
		return img2.getSrc();
	}

}