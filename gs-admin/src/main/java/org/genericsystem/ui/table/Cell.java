package org.genericsystem.ui.table;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.GenericRow;

public class Cell<T> extends Stylable {

	private final ObservableValue<T> observableModel;
	private Property<String> newValModel = new SimpleObjectProperty<String>();

	public Cell(ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		super(styleClass);
		this.observableModel = observableModel;
	}

	public ObservableValue<T> getObservableModel() {
		return observableModel;
	}

	public Property<String> getNewValModel() {
		if (observableModel.getValue() != null)
			newValModel.setValue(observableModel.getValue() + "");
		return newValModel;
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

	public Property<String> getValue() {
		return new SimpleStringProperty("" + (observableModel.getValue()));
	}

	public void update() {
		((GenericRow) getParent()).update(newValModel.getValue());
	}

	public void delete() {
		((GenericRow) getParent()).delete();
	}
}
