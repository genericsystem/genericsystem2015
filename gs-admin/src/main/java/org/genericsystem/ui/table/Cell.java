package org.genericsystem.ui.table;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
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

	public Property<Number> getLastColumnCellsWidth() {
		return ((Table) getParent().getParent()).getLastColumnWidth();
	}

	public Property<Number> getFirstColumnCellsWidth() {
		return ((Table) getParent().getParent()).getFirstColumnWidth();
	}

	public Property<Number> getColumnCellsWidth() {
		return ((Table) getParent().getParent()).getColumnWidth();
	}

	public Property<String> getName() {
		return new SimpleStringProperty("Test");
	}
}
