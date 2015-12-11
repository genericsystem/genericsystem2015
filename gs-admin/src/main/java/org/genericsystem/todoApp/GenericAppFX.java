package org.genericsystem.todoApp;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.stage.Stage;

import org.genericsystem.todoApp.AbstractGenericList.GenericList;

public class GenericAppFX extends Application {

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void start(Stage stage) throws Exception {
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Reactive Example");
		new GenericList().initTable(((Group) scene.getRoot()));
		stage.setScene(scene);
		stage.show();
	}
}
