package org.genericsystem.distributed.cacheonserver.ui.exemple;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model.EngineModel;

public class CarModel extends EngineModel {

	private Generic car;

	private Property<String> carString;
	private ObservableValue<PowerValueModel> powerValueModel;

	CarModel(CarListModel parentModel, Generic car) {
		this.car = car;
		carString = new ReadOnlyObjectWrapper<>(Objects.toString(car));
		powerValueModel = new ReadOnlyObjectWrapper<PowerValueModel>(new PowerValueModel());
	}

	public Property<String> getCarString() {
		return carString;
	}

	public ObservableValue<PowerValueModel> getPowerValueModel() {
		return powerValueModel;
	}

	public void remove() {
		car.remove();
	}

	public Generic getCar() {
		return car;
	}

}
