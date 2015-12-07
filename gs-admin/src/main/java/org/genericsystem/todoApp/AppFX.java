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
		stage.setTitle("Generic System Reactive Example");
		((Group) scene.getRoot()).getChildren().add(new TodoList().init());
		stage.setScene(scene);
		stage.show();
	}
}
