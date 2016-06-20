package org.genericsystem.examplespring.bean;

public class CarBean {

	private String carName;
	private String carColor;
	private Integer carPower;

	public CarBean() {
	}

	public String getCarColor() {
		return carColor;
	}

	public void setCarColor(String carColor) {
		this.carColor = carColor;
	}

	public String getCarName() {
		return carName;
	}

	public void setCarName(String carName) {
		this.carName = carName;
	}

	public Integer getCarPower() {
		return carPower;
	}

	public void setCarPower(Integer carPower) {
		this.carPower = carPower;
	}

}
