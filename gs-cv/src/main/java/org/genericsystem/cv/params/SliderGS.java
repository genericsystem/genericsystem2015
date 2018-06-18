package org.genericsystem.cv.params;
import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.control.Slider;
import javafx.scene.control.Label;

@SuppressWarnings("restriction")
public class SliderGS extends Application {

	final Slider sliderGS = new Slider(0,1,0.5);
	final Label sliderValueCaption = new Label("Slider Value:");
	final Label sliderValue = new Label(Double.toString(sliderGS.getValue()));
	
	final static Color textColor = Color.BLACK;
	
	
	@Override
	public void start(Stage primaryStage) throws Exception {
		// TODO Auto-generated method stub
		Group root = new Group();
        Scene scene = new Scene(root, 600, 400);
        primaryStage.setScene(scene);
        primaryStage.setTitle("Slider Sample");
        scene.setFill(Color.WHITE);
	}

	public static void main(String[] args) {
		launch(args);
	}
	
}
