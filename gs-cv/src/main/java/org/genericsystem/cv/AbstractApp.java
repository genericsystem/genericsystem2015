package org.genericsystem.cv;

import org.genericsystem.cv.utils.NativeLibraryLoader;

import javafx.application.Application;
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

}
