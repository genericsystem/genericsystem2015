package org.genericsystem.ui.table;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;
import org.genericsystem.gsadmin.GenericWindow;
import org.genericsystem.gsadmin.GenericRowBuilders.*;

public abstract class TableBuilder<ITEM, COL, T> implements Builder {

	public Table build(ObservableList<ITEM> items, ObservableValue<String> firstRowFirstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> firstRowExtractor, ObservableValue<String> firstRowLastColumnString,
			Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor, TableStyle tableStyle) {
		return new Table(getFirstElement(firstRowFirstColumnString, columns, firstRowExtractor, firstRowLastColumnString, firstColumnExtractor, lastColumnExtractor, tableStyle), getElements(items, firstColumnExtractor, columns, rowColumnExtractor, lastColumnExtractor, tableStyle), getStyle(tableStyle));
	}

	protected ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.table;
	}

	protected ObservableValue<Row> getFirstElement(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> firstRowExtractor, ObservableValue<String> firstRowLastColumnString, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor,
			TableStyle tableStyle) {
		return firstRowExtractor != null ? new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(null, firstColumnExtractor != null ? firstColumnString : new SimpleStringProperty(), columns, firstRowExtractor, lastColumnExtractor != null ? firstRowLastColumnString : new SimpleStringProperty(), tableStyle))
				: new SimpleObjectProperty<>();
	}

	protected ObservableList<Row> getElements(ObservableList<ITEM> items, Function<ITEM, ObservableValue<String>> firstColumnExtractor, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor, TableStyle tableStyle) {
		return new Transformation<Row, ITEM>(items, item -> getRowBuilder().build(item, firstColumnExtractor == null ? new SimpleStringProperty() : firstColumnExtractor.apply(item), columns, col -> rowColumnExtractor.apply(item).apply(col),
				lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item), tableStyle));
	}

	protected abstract RowBuilder<COL, T> getRowBuilder();

	protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefHeight();

	protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefWidth();

}