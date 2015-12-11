package org.genericsystem.todoApp;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class AppFX extends Application {

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void start(Stage stage) throws Exception {

		Scene scene = new Scene(new Group());
		scene.getStylesheets().add(getClass().getResource("css/stylesheet.css").toExternalForm());
		stage.setTitle("Generic System Reactive Example");
		new TodoList().init(((Group) scene.getRoot()));
		stage.setScene(scene);
		stage.show();
	}
}
