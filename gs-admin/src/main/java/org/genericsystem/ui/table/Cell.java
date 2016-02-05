package org.genericsystem.ui.table;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.GenericRow;

public class Cell<T> extends Stylable {

	private final Property<T> observableModel = new SimpleObjectProperty<>();

	public Cell(ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		super(styleClass);
		this.observableModel.setValue(observableModel.getValue());
		System.out.println(observableModel.getValue().getClass());
	}

	public Property<T> getObservableModel() {
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

	// public Property<String> getName() {
	// return new SimpleStringProperty("Test");
	// }

	public void delete() {
		((GenericRow) getParent()).delete();
	}
}
