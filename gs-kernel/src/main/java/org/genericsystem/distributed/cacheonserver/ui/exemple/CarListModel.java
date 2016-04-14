package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model.EngineModel;
import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */

public class CarListModel extends EngineModel {

	private final Engine engine;
	private final Property<String> carString = new SimpleStringProperty();
	private final Property<String> powerString = new SimpleStringProperty();
	private final ObservableList<CarModel> carModels;

	public CarListModel(Engine engine) {
		this.engine = engine;
		carModels = new Transformation2<>(engine.find(Car.class).getObservableSubInstances(), g -> new CarModel(this, g) /* , carModel -> new Observable[] { CarModel.() } */);
	}

	public void create() {
		engine.find(Car.class).addInstance(getCarString().getValue()).addHolder(engine.find(Power.class), getPowerString().getValue());
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

	@Override
	public Engine getEngine() {
		return engine;
	}

	public Property<String> getCarString() {
		return carString;
	}

	public ObservableList<CarModel> getCarModels() {
		return carModels;
	}

	public Property<String> getPowerString() {
		return powerString;
	}
}
