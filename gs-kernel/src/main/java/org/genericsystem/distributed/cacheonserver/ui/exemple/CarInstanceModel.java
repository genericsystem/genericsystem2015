package org.genericsystem.distributed.cacheonserver.ui.exemple;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class CarInstanceModel extends Model {

	private Generic car;

	private Property<String> carString;
	private ObservableValue<CarInstancePowerModel> carInstancePowerModel;

	public CarInstanceModel(Generic car, Class clazz) {
		this.car = car;
		carString = new ReadOnlyObjectWrapper<>(Objects.toString(car.getValue()));
		carInstancePowerModel = new ReadOnlyObjectWrapper<>(new CarInstancePowerModel(car, clazz));
	}

	public Property<String> getCarString() {
		return carString;
	}

	public ObservableValue<CarInstancePowerModel> getCarInstancePowerModel() {
		return carInstancePowerModel;
	}

	public void remove() {
		car.remove();
	}

	public Generic getCar() {
		return car;
	}

}
