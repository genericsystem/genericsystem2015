package org.genericsystem.gsadmin;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
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

	private CocClientEngine initGS() {
		CocServer server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, 8083, "test").addClasses(Car.class, Power.class, CarColor.class, Color.class));
		server.start();
		CocClientEngine engine = new CocClientEngine(Statics.ENGINE_VALUE, null, 8083, Car.class, Power.class, CarColor.class, Color.class);

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
		Element<Group> elt = new Element<>(Group.class);
		GenericList.init(elt);
		elt.apply(new GenericList(initGS()), scene.getRoot());
		stage.setScene(scene);
		stage.show();
	}
}
