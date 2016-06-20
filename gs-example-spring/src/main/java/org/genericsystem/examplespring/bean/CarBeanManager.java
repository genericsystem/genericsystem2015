package org.genericsystem.examplespring.bean;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;

import org.genericsystem.common.Generic;
import org.genericsystem.models.Car;
import org.genericsystem.models.CarColor;
import org.genericsystem.models.Color;
import org.genericsystem.models.Power;
import org.genericsystem.spring.Engine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("request")
public class CarBeanManager {

	@Autowired
	private Engine engine;

	private Generic car;
	private Generic power;
	private Generic color;
	private Generic carColor;

	private String newCarName;
	private Integer newCarPower;

	@PostConstruct
	public void init() {

		car = engine.find(Car.class);
		power = engine.find(Power.class);
		color = engine.find(Color.class);
		carColor = engine.find(CarColor.class);

	}

	public List<Generic> getCars() {
		return car.getSubInstances().stream().collect(Collectors.toList());
	}

	public ValueExpressionWrapper getPower(Generic instance) {
		return new ValueExpressionWrapper() {
			@Override
			public String getValue() {
				// Power is a property constraint
				return Objects.toString(instance.getValues(power).first());
			}

			@Override
			public void setValue(String value) {
				// The value power must be an integer due the InstanceValueClassConstraint
				instance.setHolder(power, Integer.parseInt(value));
			}
		};
	}

	public ValueExpressionWrapper getPower(long id) {
		return new ValueExpressionWrapper() {
			@Override
			public String getValue() {
				// Power is a property constraint
				return Objects.toString(getCarById(id).getValues(power).first());
			}

			@Override
			public void setValue(String value) {
				// The value power must be an integer due the InstanceValueClassConstraint
				getCarById(id).setHolder(power, Integer.parseInt(value));
			}
		};
	}

	public ValueExpressionWrapper getColor(Generic instance) {
		return new ValueExpressionWrapper() {

			@Override
			public void setValue(String value) {
				// The value color is a string to convert in Generic
				Generic searchedColor = color.setInstance(value);
				instance.setLink(carColor, "link", searchedColor);
			}

			@Override
			public String getValue() {
				Generic link = instance.getLinks(carColor).first();
				return (link != null) ? (String) link.getTargetComponent().getValue() : null;
			}
		};
	}

	public ValueExpressionWrapper getColor(long id) {
		return new ValueExpressionWrapper() {

			@Override
			public void setValue(String value) {
				// The value color is a string to convert in Generic
				Generic searchedColor = color.setInstance(value);
				getCarById(id).setLink(carColor, "link", searchedColor);
			}

			@Override
			public String getValue() {
				Generic link = getCarById(id).getLinks(carColor).first();
				return (link != null) ? (String) link.getTargetComponent().getValue() : null;
			}
		};
	}

	public void addCar() {
		car.setInstance(newCarName).setHolder(power, newCarPower);
	}

	public void addCar(CarBean carBeanModel) {
		Generic t = car.setInstance(carBeanModel.getCarName());
		t.setHolder(power, carBeanModel.getCarPower());
		this.getColor(t).setValue(carBeanModel.getCarColor());
	}

	public void deleteCar(Generic car) {
		car.remove();
	}

	public void deleteCarById(long id) {
		engine.getGenericById(id).remove();
	}

	public Generic getCarById(long id) {
		return engine.getGenericById(id);
	}

	public void flush() {
		engine.getCurrentCache().flush();
	}

	public void clear() {
		engine.getCurrentCache().clear();
	}

	public static interface ValueExpressionWrapper {
		public String getValue();

		public void setValue(String value);
	}

	public String getNewCarName() {
		return newCarName;
	}

	public void setNewCarName(String newCarName) {
		this.newCarName = newCarName;
	}

	public Integer getNewCarPower() {
		return newCarPower;
	}

	public void setNewCarPower(Integer newCarPower) {
		this.newCarPower = newCarPower;
	}

}
