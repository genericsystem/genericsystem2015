package org.genericsystem.cv;

import org.genericsystem.cv.params.ParamsPanel;
import org.genericsystem.cv.utils.NativeLibraryLoader;

import javafx.application.Application;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.ScrollPane;
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

	private ParamsPanel paramsPanel = new ParamsPanel();

	public void addDoubleSliderProperty(String propertyName, DoubleProperty property, double min, double max) {
		DoubleProperty sliderProperty = paramsPanel.addSliderProperty(propertyName, property.getValue(), min, max);
		property.bind(sliderProperty);
	}

	public void addIntegerSliderProperty(String propertyName, IntegerProperty property, double min, double max) {
		DoubleProperty sliderProperty = paramsPanel.addSliderProperty(propertyName, property.getValue(), min, max);
		property.bind(sliderProperty);
	}

	@Override
	public void start(Stage stage) throws Exception {
		GridPane gridPane = new GridPane();
		fillGrid(gridPane);
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Information Retriever");
		ScrollPane scrollPane = new ScrollPane(gridPane);
		scrollPane.setFitToHeight(true);
		VBox root = new VBox(scrollPane, paramsPanel);
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
			if (event.getCode() == KeyCode.S)
				onS();
			if (event.getCode() == KeyCode.P)
				onSpace();
		});
		stage.setScene(scene);
		stage.show();
	}

	protected void onS() {
		System.out.println("s pressed");
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

	protected long trace(String message, long ref) {
		long last = System.currentTimeMillis();
		System.out.println(message + " : " + (last - ref));
		return last;
	}
}
