package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.util.Callback;

import org.genericsystem.ui.Element;

public class GSTableColumn<T> extends Element<TableColumn> {

	public <M> GSTableColumn(Element parent, String columnTitle, Function<M, String> stringConverter) {
		super(parent, TableColumn.class, TableView<M>::getColumns);
		setText(columnTitle);
		setCellValueFactory(features -> new SimpleObjectProperty<>(stringConverter.apply((M) features.getValue())));
	}

	public <M> GSTableColumn(Element parent, Function<T, ObservableValue<String>> columnTitleObservable, Function<M, String> stringConverter) {
		super(parent, TableColumn.class, TableView<M>::getColumns);
		setObservableText(columnTitleObservable);
		setCellValueFactory(features -> new SimpleObjectProperty<>(stringConverter.apply((M) features.getValue())));
	}

	public GSTableColumn<T> setCellValueFactory(Callback<CellDataFeatures<T, String>, ObservableValue<String>> valueFactory) {
		super.addBoot(TableColumn::cellValueFactoryProperty, valueFactory);
		return this;
	}

	public GSTableColumn<T> setPrefWidth(Number prefWidth) {
		super.addBoot(TableColumn::prefWidthProperty, prefWidth);
		return this;
	}

	public GSTableColumn<T> setObservableText(Function<T, ObservableValue<String>> columnTitleObservable) {
		addBinding(TableColumn::textProperty, columnTitleObservable);
		return this;
	}

	public GSTableColumn<T> setText(String columnTitle) {
		addBoot(TableColumn::textProperty, columnTitle);
		return this;
	}
}
