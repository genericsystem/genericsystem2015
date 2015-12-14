package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import org.genericsystem.ui.Element;

public class GSTableColumn extends Element<TableColumn> {

	private static Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;

	// Callback<CellDataFeatures<T, String>, ObservableValue<String>> callback;

	public GSTableColumn(Element parent) {
		super(parent, TableColumn.class, getColumns);
		// callback = features -> new SimpleObjectProperty<>(features.getValue().toString());
	}

	public GSTableColumn setWidth(Number width) {
		super.addBoot(TableColumn::prefWidthProperty, width);
		return this;
	}

	public <T> GSTableColumn setCellValueFactoryProperty(T value) {
		super.addBoot(TableColumn::cellValueFactoryProperty, value);
		return this;
	}

	public <T> GSTableColumn setCellFactoryProperty(T value) {
		super.addBoot(TableColumn::cellFactoryProperty, value);
		return this;
	}

	public <T> GSTableColumn setObservableTextProperty(Function<T, ObservableValue<String>> function) {
		addBinding(TableColumn::textProperty, function);
		return this;
	}

	public <T> GSTableColumn setText(String value) {
		addBoot(TableColumn::textProperty, value);
		return this;
	}

}
