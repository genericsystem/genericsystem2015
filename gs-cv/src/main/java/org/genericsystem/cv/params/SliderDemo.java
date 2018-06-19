package org.genericsystem.cv.params;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class SliderDemo extends Application {

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void start(Stage primaryStage) throws Exception {

		ParamsPanel layout = new ParamsPanel();
		layout.addSliderProperty("vBlurSize", 0.5, 0, 1);
		layout.addSliderProperty("hBlurSize", 0.5, 0, 1);
		primaryStage.setScene(new Scene(layout));
		primaryStage.setTitle("Slider Sample");
		primaryStage.show();
	}

}
