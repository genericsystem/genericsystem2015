package org.genericsystem.gui.app;

import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
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
import org.genericsystem.newgui.component.IComponent;
import org.genericsystem.newgui.metacomponent.IMetaComponent;
import org.genericsystem.newgui.metacomponent.IMetaComponent.ButtonMetaComponent;
import org.genericsystem.newgui.metacomponent.IMetaComponent.TableViewMetaComponent;
import org.genericsystem.newgui.metacomponent.IMetaComponent.VBoxMetaComponent;
import org.genericsystem.newgui.metacontext.IMetaContext;
import org.genericsystem.newgui.metacontext.IMetaContext.RootMetaContext;

public class App extends Application {

	public static void main(String args[]) {
		CocServer server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, 8082, "test").addClasses(Car.class, Power.class, CarColor.class, Color.class));
		server.start();
		launch(args);
		// server.stop();
	}

	@Override
	public void start(Stage stage) {
		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System JavaFx Example");
		// RootContext rootContext = new RootContext(initGS());

		IMetaContext rootMetaContext = new RootMetaContext(null);
		// IMetaContext tableViewMetaContext = new TableViewMetaContext(rootMetaContext);

		IMetaComponent vBoxMetaComponent = new VBoxMetaComponent(null);
		IMetaComponent tableViewMetaComp = new TableViewMetaComponent(vBoxMetaComponent);
		IMetaComponent buttonMetaComp = new ButtonMetaComponent(vBoxMetaComponent);

		IComponent rootComponent = vBoxMetaComponent.apply(rootMetaContext, null);

		// vBoxMetaComponent.getChildren().forEach(metaComponent -> metaComponent.apply(rootMetaContext, rootComponent));

		// IComponent tableViewComponent = tableViewMetaComp.apply(rootMetaContext, rootComponent);
		// IComponent buttonComponent = buttonMetaComp.apply(rootMetaContext, rootComponent);

		VBox root = (VBox) rootComponent.getNode();

		((Group) scene.getRoot()).getChildren().add(root);
		stage.setScene(scene);
		stage.show();
	}

	private CocClientEngine initGS() {
		CocClientEngine engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, Car.class, Power.class, CarColor.class, Color.class);
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
		base2.setLink(relation, "myMercedesYellow", engine.find(Yellow.class));
		engine.getCurrentCache().flush();

		return engine;

	}
}
