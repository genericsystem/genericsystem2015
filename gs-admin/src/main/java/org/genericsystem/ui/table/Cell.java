package org.genericsystem.ui.table;

import javafx.beans.value.ObservableValue;

import org.genericsystem.ui.Model;

public class Cell<T> extends Stylable {

	private ObservableValue<T> observableModel;

	public Cell(Model parent, ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		super(parent, styleClass);
		this.observableModel = observableModel;
	}

	public ObservableValue<T> getObservableModel() {
		return observableModel;
	}

	public void setObservableModel(ObservableValue<T> observableModel) {
		this.observableModel = observableModel;
	}
}
