package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

public class Cell<T> extends Stylable {

	private ObservableValue<T> observableModel;

	public Cell(ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		super(styleClass);
		this.observableModel = observableModel;
	}

	public ObservableValue<T> getObservableModel() {
		return observableModel;
	}

	public void setObservableModel(ObservableValue<T> observableModel) {
		this.observableModel = observableModel;
	}
}
