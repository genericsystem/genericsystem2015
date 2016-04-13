package org.genericsystem.distributed.cacheonserver.todocarmvc;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model.EngineModel;

public class CarModel extends EngineModel {

	private Generic car;

	private Property<String> carString;
	private ObservableValue<PowerValueModel> powerModel;

	CarModel(CarListModel parentModel, Generic car) {
		this.car = car;
		carString = new ReadOnlyObjectWrapper<>(Objects.toString(car));
	}

	/*
	 * public String addCar() { carModel.setInstance(newCarName).setHolder(power, newCarPower); return "#"; }
	 */

	public Property<String> getCarString() {
		return carString;
	}

	public ObservableValue<PowerValueModel> getPowerModel() {
		return powerModel;
	}

	/*
	 * public void select() { ((CarListModel) getParent()).getSelection().setValue(this); }
	 */

	public void remove() {
		car.remove();
	}

	public Generic getCar() {
		return car;
	}

}
