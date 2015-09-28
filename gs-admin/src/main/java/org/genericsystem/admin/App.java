package org.genericsystem.admin;

import javafx.application.Application;
import javafx.beans.property.SimpleObjectProperty;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.stage.Stage;

import org.genericsystem.admin.UiFunctions.GsUiFunctions;
import org.genericsystem.admin.model.Car;
import org.genericsystem.admin.model.CarColor;
import org.genericsystem.admin.model.Color;
import org.genericsystem.admin.model.Color.Red;
import org.genericsystem.admin.model.Color.Yellow;
import org.genericsystem.admin.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.LightClientEngine;
import org.genericsystem.distributed.WebSocketGSHeavyServer;
import org.genericsystem.javafx.Crud;
import org.genericsystem.kernel.Statics;

/**
 * @author Nicolas Feybesse
 *
 */
public class App extends Application {

	public static void main(String args[]) {
		WebSocketGSHeavyServer server = new WebSocketGSHeavyServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, 8082, "test2").addClasses(Car.class, Power.class, CarColor.class, Color.class));
		server.start();
		launch(args);
		// server.stop();
	}

	@Override
	public void start(Stage stage) {

		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System JavaFx Example");

		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, Car.class, Power.class, CarColor.class, Color.class);

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
		class InvalidableObjectProperty extends SimpleObjectProperty<Generic> {
			public InvalidableObjectProperty(Generic engine) {
				super(engine);
			}

			@Override
			protected void fireValueChangedEvent() {
				super.fireValueChangedEvent();
			}
		}
		InvalidableObjectProperty engineProperty = new InvalidableObjectProperty(engine);
		Thread.currentThread().setUncaughtExceptionHandler((thread, throwable) -> {
			engineProperty.fireValueChangedEvent();
			Alert alert = new Alert(AlertType.WARNING);
			alert.setTitle("A problem was detected");
			alert.setContentText(throwable.getCause() != null ? throwable.getCause().getMessage() : throwable.getMessage());
			alert.setHeaderText(throwable.getClass().getSimpleName());
			throwable.printStackTrace();
			alert.showAndWait();
		});

		Crud<Generic> crud = new Crud<>(engineProperty, new GsUiFunctions());

		((Group) scene.getRoot()).getChildren().add(crud);
		stage.setScene(scene);
		stage.show();
	}
	// public static abstract class GsList extends AbstractSet<Generic> {
	//
	// private final Snapshot<Generic> dependencies;
	//
	// private long currentTs = Integer.MIN_VALUE;
	// private List<Generic> currentCache;
	// private int currentIndex;
	// private Iterator<Generic> currentDependenciesIterator;
	//
	// public GsList(Snapshot<Generic> dependencies) {
	// this.dependencies = dependencies;
	// }
	//
	// public abstract long getTs();
	//
	// private void checkTs() {
	// long ts = getTs();
	// if (ts == currentTs)
	// return;
	// this.currentTs = ts;
	// this.currentCache = new ArrayList<Generic>();
	// this.currentIndex = -1;
	// this.currentDependenciesIterator = dependencies.iterator();
	// }
	//
	// private void fillCacheToIndexIfNecessary(int index) {
	// if (index <= currentIndex)
	// return;
	// while (index > currentIndex) {
	// if (!currentDependenciesIterator.hasNext())
	// throw new IndexOutOfBoundsException("" + index);
	// currentCache.add(currentDependenciesIterator.next());
	// currentIndex++;
	// }
	// }
	//
	// public Generic get(int index) {
	// checkTs();
	// fillCacheToIndexIfNecessary(index);
	// return currentCache.get(index);
	// }
	//
	// private void completeCache() {
	// while (currentDependenciesIterator.hasNext()) {
	// currentCache.add(currentDependenciesIterator.next());
	// currentIndex++;
	// }
	// }
	//
	// @Override
	// public int size() {
	// checkTs();
	// completeCache();
	// return currentCache.size();
	// }
	//
	// @Override
	// public boolean add(Generic e) {
	// // TODO Auto-generated method stub
	// return super.add(e);
	// }
	//
	// @Override
	// public boolean remove(Object o) {
	// // TODO Auto-generated method stub
	// return super.remove(o);
	// }
	//
	// @Override
	// public Generic remove(int index) {
	// checkTs();
	// Generic generic = get(index);
	// currentCache.remove(generic);
	// generic.remove();
	// return generic;
	// }
	// }
}
