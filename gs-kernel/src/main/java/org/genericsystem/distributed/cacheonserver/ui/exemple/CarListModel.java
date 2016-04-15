package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */

public class CarListModel extends Model {

	private final Engine engine;
	private Generic car;
	private final Property<String> carString = new SimpleStringProperty();
	private final Property<String> powerString = new SimpleStringProperty();
	private final ObservableList<CarInstanceModel> carInstanceModels;

	public CarListModel(Engine engine, Class carClass, Class powerClass) {
		this.engine = engine;
		this.car = engine.find(carClass);
		carInstanceModels = new Transformation2<>(car.getObservableSubInstances(), g -> new CarInstanceModel(g, powerClass) /* , carModel -> new Observable[] { CarModel.() } */);
	}

	public void create() {
		car.addInstance(getCarString().getValue()).addHolder(engine.find(Power.class), getPowerString().getValue());
		carString.setValue(null);
		powerString.setValue(null);
	}

	/********************/
	public void flush() {
		engine.getCurrentCache().flush();
	}

	public void cancel() {
		engine.getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public Property<String> getCarString() {
		return carString;
	}

	public ObservableList<CarInstanceModel> getCarInstanceModels() {
		return carInstanceModels;
	}

	public Property<String> getPowerString() {
		return powerString;
	}
}
