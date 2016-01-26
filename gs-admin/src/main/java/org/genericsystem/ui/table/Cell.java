package org.genericsystem.ui.table;

import javafx.beans.value.ObservableValue;

public class Cell<T> extends Stylable {

	private final ObservableValue<T> observableModel;

	public Cell(ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		super(styleClass);
		this.observableModel = observableModel;
	}

	public ObservableValue<T> getObservableModel() {
		return observableModel;
	}
}
