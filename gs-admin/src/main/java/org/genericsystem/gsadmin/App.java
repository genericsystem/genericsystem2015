package org.genericsystem.gsadmin;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.stage.Stage;

import org.genericsystem.admin.model.Car;
import org.genericsystem.admin.model.CarColor;
import org.genericsystem.admin.model.Color;
import org.genericsystem.admin.model.Color.Red;
import org.genericsystem.admin.model.Color.Yellow;
import org.genericsystem.admin.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.distributed.cacheonclient.CocServer;
import org.genericsystem.kernel.Statics;
import org.genericsystem.ui.Element;

public class App extends Application {

	public static void main(String[] args) {
		launch(args);
	}

	static CocClientEngine engine;
	static CocServer server;
	private CocClientEngine initGS() {
		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, 8082, "test").addClasses(Car.class, Power.class, CarColor.class, Color.class));
		server.start();
		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, 8082, Car.class, Power.class, CarColor.class, Color.class);

		Generic type = engine.find(Car.class);

		Generic base = type.setInstance("myBmw");
		assert base.isAlive();
		type.setInstance("myAudi");
		type.setInstance("myMercedes");

		Generic attribute = engine.find(Power.class);
		Generic relation = engine.find(CarColor.class);
		base.setHolder(attribute, 333);
		base.setLink(relation, "myBmwRed", engine.find(Red.class));
		base.setLink(relation, "myBmwYellow", engine.find(Yellow.class));
		Generic base2 = type.setInstance("myMercedes");
		base2.setHolder(attribute, 333);
		base2.setLink(relation, "myMercedesYellow", engine.find(Yellow.class));
		engine.getCurrentCache().flush();
		return engine;
	}

	int i = 0;

	@Override
	public void start(Stage stage) throws Exception {
//		Button addGen = new Button("Add");
//		addGen.setOnAction(e -> {
//			engine.addInstance("*********************************" + i);
//			i++;
//		});

		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Reactive Example");
		scene.getStylesheets().add(getClass().getResource("css/stylesheet.css").toExternalForm());
		Element<Group> elt = new Element<>(Group.class);
		WindowBuilder builder = new WindowBuilder();
		builder.init(elt);// Do this only one time
		Window window = builder.buildWithGeneric(scene.widthProperty(), scene.heightProperty(), initGS());
		elt.apply(window, scene.getRoot());// Do this only one time
//		((Group) scene.getRoot()).getChildren().add(addGen);
		stage.setScene(scene);
		stage.setWidth(800);
		stage.setHeight(600);
		stage.show();
		stage.setOnCloseRequest(e->server.stop());
	}
}
