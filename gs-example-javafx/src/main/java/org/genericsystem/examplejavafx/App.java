package org.genericsystem.examplejavafx;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.stage.Stage;

import org.genericsystem.examplejavafx.model.Car;
import org.genericsystem.examplejavafx.model.CarColor;
import org.genericsystem.examplejavafx.model.Color;
import org.genericsystem.examplejavafx.model.Power;
import org.genericsystem.mutability.Engine;


/**
 * @author Nicolas Feybesse
 *
 */
public class App extends Application {

	public static void main(String args[]) {
		launch(args);
	}

	@Override
	public void start(Stage stage) {
		
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System JavaFx Example");
		stage.setWidth(840);
		stage.setHeight(500);
		
		Engine engine = new Engine(Car.class, Power.class, CarColor.class, Color.class);

		Crud crud = new Crud(engine.find(Car.class),engine.find(Power.class), engine.find(CarColor.class));
		((Group) scene.getRoot()).getChildren().addAll(crud);
		stage.setScene(scene);
		stage.show();
	}
}
