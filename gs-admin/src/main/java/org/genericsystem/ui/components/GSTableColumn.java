package org.genericsystem.ui.components;

import java.util.function.Function;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.util.Callback;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.Model;

public class GSTableColumn<T> extends Element<TableColumn> {
	<MODEL extends Model<?, MODEL>>
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

	// @Override
	// public <M, U> GSTableColumn<T> forEach(Function<M, ObservableList<U>> function, Function<U, Property<M>> injectedProperty) {
	// super.forEach(function, injectedProperty);
	// return this;
	// }

	@Override
	public <M, U extends Model> GSTableColumn<T> forEach(Function<M, ObservableList<U>> function) {
		super.forEach(function);
		return this;
	}
}
