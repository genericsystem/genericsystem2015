package org.genericsystem.cv.params;
import ch.qos.logback.core.net.SyslogOutputStream;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.control.Slider;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;



public class SliderGS extends Application {

	final Model model = new Model();
	
	final Slider sliderGS = new Slider(0,1,0.5);
	final Label sliderValueCaption = new Label("Slider Value:");
//	final Label sliderValue = new Label(Double.toString(sliderGS.getValue()));
	
	
	final static Color textColor = Color.BLACK;
	
	public static class Model{
		
		public DoubleProperty sliderValue = new SimpleDoubleProperty();
		
		public void setSliderValue(DoubleProperty sliderValue) {
			this.sliderValue = sliderValue;
		}
		public DoubleProperty getSliderValue() {
			return sliderValue;
		}
	}
	
	@Override
	public void start(Stage primaryStage) throws Exception {
		// TODO Auto-generated method stub
		VBox layout = new VBox(10);
        primaryStage.setScene(new Scene(layout));
        primaryStage.setTitle("Slider Sample");
                
        primaryStage.show();
	}

	public static void main(String[] args) {
		launch(args);
	}
	
}
