package org.genericsystem.distributed.cacheonserver.todocarmvc;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;

import org.genericsystem.distributed.ui.Model.EngineModel;

public class PowerValueModel extends EngineModel {

	private Property<String> powerValueString;

	// public PowerValueModel() {
	// powerValueString = new ReadOnlyObjectWrapper<>(Objects.toString(((CarModel) getParent()).getCar().getHolder(getEngine().find(Power.class)).getValue()));
	// }

	@Override
	public void afterParentConstruct() {
		assert ((CarModel) getParent()).getCar() != null;
		powerValueString = new ReadOnlyObjectWrapper<>(Objects.toString(((CarModel) getParent()).getCar().getHolder(getEngine().find(Power.class)).getValue()));
	}

	public Property<String> getPowerValueString() {
		return powerValueString;
	}
}
