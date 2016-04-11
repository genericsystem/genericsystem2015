package org.genericsystem.distributed.cacheonserver.todocarmvc;

import java.util.ArrayList;
import java.util.function.Predicate;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableBooleanValue;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableStringValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */
@SuppressWarnings("unchecked")
public class CarListModel extends Model {

	private final Engine engine;
	private final Property<?> engineCrud = new SimpleObjectProperty<>();

	private final Property<String> name = new SimpleStringProperty();
	private final Property<Predicate<CarModel>> mode = new SimpleObjectProperty<>(ALL);
	private final ObservableList<CarModel> carModels;// = FXCollections.<CarModel> observableArrayList(CarModel -> new Observable[] { CarModel.getCompleted() });
	private final FilteredList<CarModel> filtered;// = new FilteredList<>(CarModels);
	private final ObservableNumberValue completedCount;// = Bindings.size(CarModels.filtered(COMPLETE));
	private final ObservableNumberValue activeCount;// = Bindings.size(CarModels.filtered(ACTIVE));
	private final ObservableValue<String> clearButtonText;// = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
	private final ObservableValue<Boolean> hasNoCompleted;// = Bindings.equal(0, completedCount);
	private final ObservableBooleanValue hasCarModel;// = Bindings.lessThan(0, Bindings.size(CarModels));
	private final ObservableValue<Boolean> hasNoCarModel;// = Bindings.not(hasCarModel);
	private final ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue<Predicate<CarModel>>) mode, ALL);
	private final ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue<Predicate<CarModel>>) mode, ACTIVE);
	private final ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue<Predicate<CarModel>>) mode, COMPLETE);
	private final Property<CarModel> selection = new SimpleObjectProperty<>();
	private final ObservableStringValue items;// = Bindings.createStringBinding(() -> " " + (activeCount.getValue().intValue() > 1 ? "items" : "item") + " left", activeCount);
	private final ObservableStringValue clearCompleted;// = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue().intValue() + ")", completedCount);

	public CarListModel(Engine engine) {
		this.engine = engine;
		ObservableList<CarModel> instances = new Transformation2<>(engine.find(Car.class).getObservableSubInstances(), g -> new CarModel(this, g));
		carModels = new Transformation2<>(engine.find(Car.class).getObservableSubInstances(), g -> new CarModel(this, g), carModel -> new Observable[] { carModel.getCompleted() });
		filtered = new FilteredList<>(carModels);
		filtered.predicateProperty().bind(Bindings.createObjectBinding(() -> mode.getValue(), mode));
		completedCount = Bindings.size(carModels.filtered(COMPLETE));
		activeCount = Bindings.size(carModels.filtered(ACTIVE));
		clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
		hasNoCompleted = Bindings.equal(0, completedCount);
		hasCarModel = Bindings.lessThan(0, Bindings.size(carModels));
		hasNoCarModel = Bindings.not(hasCarModel);
		items = Bindings.createStringBinding(() -> " " + (activeCount.getValue().intValue() > 1 ? "items" : "item") + " left", activeCount);
		clearCompleted = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue().intValue() + ")", completedCount);
	}

	public ObservableValue<String> getActiveCount() {
		return Bindings.convert(activeCount);
	}

	public ObservableValue<String> getItems() {
		return items;
	}

	public void create() {
		engine.find(Car.class).addInstance(getName().getValue());
		// engine.addInstance(getName().getValue());
		System.out.println("Add instance : " + getName().getValue());
		// CarModels.add(new CarModel(this, getName().getValue()));
		name.setValue(null);
	}

	public void showAll() {
		mode.setValue(ALL);
		System.out.println("Show All" + mode);
	}

	public void showActive() {
		mode.setValue(ACTIVE);
	}

	public void showCompleted() {
		mode.setValue(COMPLETE);
	}

	public void removeCompleted() {
		for (CarModel carModel : new ArrayList<>(carModels.filtered(COMPLETE)))
			carModel.remove();
	}

	/********************/
	public void flush() {
		engine.getCurrentCache().flush();
	}

	public void cancel() {
		engine.getCurrentCache().clear();
	}

	/***********************/

	static Predicate<CarModel> ALL = CarModel -> true;
	static Predicate<CarModel> ACTIVE = CarModel -> !CarModel.getCompleted().getValue();
	static Predicate<CarModel> COMPLETE = CarModel -> CarModel.getCompleted().getValue();

	/*********************************************************************************************************************************/
	public Engine getEngine() {
		return engine;
	}

	public Property<String> getName() {
		return name;
	}

	public Property<Predicate<CarModel>> getMode() {
		return mode;
	}

	public ObservableList<CarModel> getCarModels() {
		return carModels;
	}

	public ObservableList<CarModel> getFiltered() {
		return filtered;
	}

	public ObservableValue<Number> getCompletedCount() {
		return completedCount;
	}

	public ObservableValue<String> getClearButtonText() {
		return clearButtonText;
	}

	public ObservableValue<Boolean> getHasNoCompleted() {
		return hasNoCompleted;
	}

	public ObservableValue<Boolean> getHasCarModel() {
		return hasCarModel;
	}

	public ObservableValue<Boolean> getHasNoCarModel() {
		return hasNoCarModel;
	}

	public ObservableValue<Boolean> getAllMode() {
		return allMode;
	}

	public ObservableValue<Boolean> getActiveMode() {
		return activeMode;
	}

	public ObservableValue<Boolean> getCompletedMode() {
		return completedMode;
	}

	public Property<CarModel> getSelection() {
		return selection;
	}

	public ObservableValue<String> getClearCompleted() {
		return clearCompleted;
	};

}
