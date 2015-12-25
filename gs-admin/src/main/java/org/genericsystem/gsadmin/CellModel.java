package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.Stylable.TableStyle;

public class CellModel<T> extends Model {

	private final ObservableValue<T> observableModel;
	private final TableStyle tableStyle;

	public CellModel(ObservableValue<T> observableModel, TableStyle tableStyle) {
		this.observableModel = observableModel;
		this.tableStyle = tableStyle;
	}

	public ObservableValue<T> getObservableModel() {
		return observableModel;
	}

	@Override
	protected TableStyle getTableStyle() {
		return tableStyle;
	}

}
