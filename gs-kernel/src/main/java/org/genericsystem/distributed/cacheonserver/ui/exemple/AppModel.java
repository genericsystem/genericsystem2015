package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.kernel.Engine;

public class AppModel extends GenericModel {

	private final ObservableValue<TypeTableModel> typeModel;
	private final Property<String> carString = new SimpleStringProperty();

	private final Property<String> powerString = new SimpleStringProperty();

	public AppModel(Engine engine, Generic type, ObservableList<Generic> attributes) {
		super(engine);
		typeModel = new ReadOnlyObjectWrapper<TypeTableModel>(new TypeTableModel(type, attributes));
	}

	public void create() {
		getGeneric().addInstance(getString().getValue()).addHolder(getGeneric().find(Power.class), getPowerString().getValue());
		carString.setValue(null);
		powerString.setValue(null);
	}

	public Property<String> getCarString() {
		return carString;
	}

	@Override
	protected Engine getGeneric() {
		return (Engine) super.getGeneric();
	}

	public Property<String> getPowerString() {
		return powerString;
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public ObservableValue<TypeTableModel> getTypeModel() {
		return typeModel;
	}
}
