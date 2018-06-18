package org.genericsystem.cv.params;

import javafx.application.Application;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.Slider;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

@SuppressWarnings("restriction")
public class SliderTest extends Application {
	private final ListView<String> startLog = new ListView<>();
	private final ListView<String> endLog = new ListView<>();

	@Override
	public void start(Stage stage) throws Exception {
		Pane logsPane = createLogsPane();
		Slider slider = createMonitoredSlider();

		VBox layout = new VBox(10);
		layout.setAlignment(Pos.CENTER);
		layout.setPadding(new Insets(10));
		layout.getChildren().setAll(slider, logsPane);
		VBox.setVgrow(logsPane, Priority.ALWAYS);

		stage.setTitle("Slider Value Change Logger");
		stage.setScene(new Scene(layout));
		stage.show();
	}

	private Slider createMonitoredSlider() {
		Slider slider = new Slider(0, 1, 0.5);
		slider.setMajorTickUnit(0.5);
		slider.setMinorTickCount(0);
		slider.setShowTickMarks(true);
		slider.setShowTickLabels(true);
		slider.setMinHeight(Slider.USE_PREF_SIZE);

		slider.valueChangingProperty().addListener(new ChangeListener<Boolean>() {
			@Override
			public void changed(ObservableValue<? extends Boolean> observableValue, Boolean wasChanging,
					Boolean changing) {
				String valueString = String.format("%1$.3f", slider.getValue());

				if (changing) {
					startLog.getItems().add(valueString);
				} else {
					endLog.getItems().add(valueString);
				}
			}
		});
		return slider;
	}

	private HBox createLogsPane() {
		HBox logs = new HBox(10);
		logs.getChildren().addAll(createLabeledLog("Start", startLog), createLabeledLog("End", endLog));
		return logs;
	}

	public Pane createLabeledLog(String logName, ListView<String> log) {
		Label label = new Label(logName);
		label.setLabelFor(log);

		VBox logPane = new VBox(5);
		logPane.getChildren().setAll(label, log);

		logPane.setAlignment(Pos.TOP_LEFT);

		return logPane;
	}

	public static void main(String[] args) {
		launch(args);
	}
}