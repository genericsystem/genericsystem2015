package org.genericsystem.ui.components;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.util.Callback;

import org.genericsystem.todoApp.DeleteButtonCell;
import org.genericsystem.ui.Element;

public class GSTableColumn<T> extends Element<TableColumn> {

	protected static Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;
	private Callback<CellDataFeatures<T, String>, ObservableValue<String>> callback;

	public GSTableColumn(Element parent, Function<T, String> function) {
		super(parent, TableColumn.class, getColumns);
		callback = features -> new SimpleObjectProperty<>(function.apply(features.getValue()));
		super.addBoot(TableColumn::cellValueFactoryProperty, callback);
	}

	public GSTableColumn setWidth(Number width) {
		super.addBoot(TableColumn::prefWidthProperty, width);
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

	public static class GSTableColumnAction<T> extends GSTableColumn<T> {
		private Callback<TableColumn<T, String>, TableCell<T, String>> callbackDelete;

		public GSTableColumnAction(Element parent, Function<T, String> function, Consumer<T> consumer) {
			super(parent, function);
			callbackDelete = col -> new DeleteButtonCell<>(consumer);
			super.addBoot(TableColumn::cellFactoryProperty, callbackDelete);
		}

	}

}
