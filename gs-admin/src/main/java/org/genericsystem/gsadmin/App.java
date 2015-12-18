package org.genericsystem.gsadmin;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.stage.Stage;

import org.genericsystem.ui.Element;

public class App extends Application {

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void start(Stage stage) throws Exception {
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Reactive Example");
		scene.getStylesheets().add(getClass().getResource("css/stylesheet.css").toExternalForm());

		Element<Group> elt = new Element<>(Group.class);
		GenericList.init(elt);
		elt.apply(new GenericList(), scene.getRoot());
		stage.setScene(scene);
		stage.show();
	}
}
