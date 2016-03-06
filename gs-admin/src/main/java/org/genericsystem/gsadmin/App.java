package org.genericsystem.gsadmin;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.stage.Screen;
import javafx.stage.Stage;

import org.genericsystem.admin.model.Car;
import org.genericsystem.admin.model.CarColor;
import org.genericsystem.admin.model.Color;
import org.genericsystem.admin.model.Color.Red;
import org.genericsystem.admin.model.Color.Yellow;
import org.genericsystem.admin.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.BackEnd;
import org.genericsystem.distributed.cacheonclient.ClientEngine;
import org.genericsystem.kernel.Statics;

public class App extends Application {

	public static void main(String[] args) {
		launch(args);
	}

	static ClientEngine engine;
	static BackEnd server;

	private ClientEngine initGS() {
		server = new BackEnd(new GSDeploymentOptions(Statics.ENGINE_VALUE, 8082, "test").addClasses(Car.class, Power.class, CarColor.class, Color.class));
		server.start();
		engine = new ClientEngine(Statics.ENGINE_VALUE, null, 8082, Car.class, Power.class, CarColor.class, Color.class);

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

	@Override
	public void start(Stage stage) throws Exception {

		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System Reactive Example");
		scene.getStylesheets().add(getClass().getResource("css/stylesheet.css").toExternalForm());

		stage.setScene(scene);
		stage.setWidth(Screen.getPrimary().getVisualBounds().getWidth());
		stage.setHeight(Screen.getPrimary().getVisualBounds().getHeight());

		new GSAdmin(GenericWindow.createWindow(stage.widthProperty(), stage.heightProperty(), initGS()), (Group) scene.getRoot());

		stage.show();
		stage.setOnCloseRequest(e -> {
			server.stop();
			Platform.exit();
			System.exit(0);
		});

	}
}
