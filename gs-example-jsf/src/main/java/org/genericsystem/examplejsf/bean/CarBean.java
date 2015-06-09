package org.genericsystem.examplejsf.bean;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.cdi.Engine;
import org.genericsystem.examplejsf.model.Car;
import org.genericsystem.examplejsf.model.CarColor;
import org.genericsystem.examplejsf.model.Color;
import org.genericsystem.examplejsf.model.Power;
import org.genericsystem.mutability.Generic;

@Named
@RequestScoped
public class CarBean {

	@Inject
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

	public String addCar() {
		car.setInstance(newCarName).setHolder(power, newCarPower);
		return "#";
	}

	public String update() {
		return "#";
	}

	public String deleteCar(Generic car) {
		car.remove();
		return "#";
	}

	public String flush() {
		engine.getCurrentCache().flush();
		return "#";
	}

	public String clear() {
		engine.getCurrentCache().clear();
		return "#";
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
