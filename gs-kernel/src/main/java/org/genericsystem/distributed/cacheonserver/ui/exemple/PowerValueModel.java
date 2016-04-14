package org.genericsystem.distributed.cacheonserver.ui.exemple;

import java.util.Objects;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model.EngineModel;

public class PowerValueModel extends EngineModel {

	private ObservableValue<String> powerValueString;

	@Override
	public void afterParentConstruct() {
		ObservableList<Generic> holders = ((CarModel) getParent()).getCar().getObservableHolders(getEngine().find(Power.class));
		powerValueString = Bindings.createStringBinding(() -> Objects.toString(((CarModel) getParent()).getCar().getHolder(getEngine().find(Power.class))), holders);
	}

	public ObservableValue<String> getPowerValueString() {
		return powerValueString;
	}
}
