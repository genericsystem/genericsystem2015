package org.genericsystem.distributed.cacheonserver.todocarmvc;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
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
	private final Property<String> name = new SimpleStringProperty();
	private final ObservableList<CarModel> carModels;
	private Property<String> powerValue = new SimpleStringProperty();
	private final Property<CarListModel> selection = new SimpleObjectProperty<>();

	public CarListModel(Engine engine) {
		this.engine = engine;
		carModels = new Transformation2<>(engine.find(Car.class).getObservableSubInstances(), g -> new CarModel(this, g) /* , carModel -> new Observable[] { CarModel.() } */);
	}

	public void create() {
		assert engine.find(Car.class) != null;
		assert engine.find(Power.class) != null;
		engine.find(Car.class).addInstance(getName().getValue()).addHolder(engine.find(Power.class), 333/* getPowerValue().getValue() */);
		System.out.println("Add instance : " + getName().getValue());
		name.setValue(null);
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

	public Property<String> getName() {
		return name;
	}

	public ObservableList<CarModel> getCarModels() {
		return carModels;
	}

	public Property<CarListModel> getSelection() {
		return selection;
	}

	public Property<String> getPowerValue() {
		return powerValue;
	}

}
