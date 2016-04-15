package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model;

public class CarInstancePowerModel extends Model {

	private final Generic carInstance;
	private final Generic power;
	private final ObservableList<PowerValueModel> powerValueModels;

	public CarInstancePowerModel(Generic carInstance, Class powerClass) {
		this.carInstance = carInstance;
		this.power = carInstance.getRoot().find(powerClass);
		powerValueModels = new Transformation2<>(carInstance.getObservableHolders(power), g -> new PowerValueModel(g) /* , carModel -> new Observable[] { CarModel.() } */);
	}

	public ObservableList<PowerValueModel> getPowerValueModels() {
		return powerValueModels;
	}
}
