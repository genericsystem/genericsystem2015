package org.genericsystem.distributed.cacheonserver.ui.exemple;

import java.util.Objects;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model.EngineModel;

public class PowerValueModel extends EngineModel {

	private final Generic holder;
	private final ObservableValue<String> powerValueString;

	public PowerValueModel(Generic holder) {
		this.holder = holder;
		powerValueString = new ReadOnlyStringWrapper(Objects.toString(holder.getValue()));

	}

	public ObservableValue<String> getPowerValueString() {
		return powerValueString;
	}
}
