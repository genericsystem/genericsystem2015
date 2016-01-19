package org.genericsystem.ui.table;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.GenericRowBuilders.TableCellRowBuilder;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellFirstRowBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilder<ITEM, COL, T> implements Builder {

	@Override
	public void init(Element<?> parent) {
		GSSCrollPane scrollPane = new GSSCrollPane(parent).setStyleClass("scrollable");
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setSuperPrefWidth(getSuperPrefWidth()).setSuperPrefHeight(getSuperPrefHeight());
			;
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass).setMinHeight(Table::getFirstRowHeight).setMaxHeight(Table::getFirstRowHeight)
						.setPrefHeight(Table::getFirstRowHeight);
				createSelectionHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass).setMinHeight(Table::getRowHeight).setMaxHeight(Table::getRowHeight).setPrefHeight(Table::getRowHeight);
			}
		}
	}

	public abstract GSHBox createSelectionHBox(Element<?> parent);

	public Table build(ObservableList<ITEM> items, ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowSecondColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> firstRowExtractor,
			ObservableValue<String> firstRowLastColumnString, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<T>> secondColumnExtractor, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor,
			Function<ITEM, ObservableValue<String>> lastColumnExtractor, TableStyle tableStyle) {
		return new Table(getFirstElement(secondColumnExtractor, firstRowFirstColumnString, firstRowSecondColumnString, columns, firstRowExtractor, firstRowLastColumnString, firstColumnExtractor, lastColumnExtractor, tableStyle), getElements(items,
				firstColumnExtractor, secondColumnExtractor, columns, rowColumnExtractor, lastColumnExtractor, tableStyle), getStyle(tableStyle));
	}

	protected ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.table;
	}

	protected ObservableValue<Row> getFirstElement(Function<ITEM, ObservableValue<T>> secondColumnExtractor, ObservableValue<String> firstColumnString, ObservableValue<String> firstRowSecondColumnString, ObservableList<COL> columns,
			Function<COL, ObservableValue<String>> firstRowExtractor, ObservableValue<String> firstRowLastColumnString, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor,
			TableStyle tableStyle) {
		return firstRowExtractor != null ? new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(null, firstColumnExtractor != null ? firstColumnString : new SimpleStringProperty(), secondColumnExtractor != null ? firstRowSecondColumnString
				: new SimpleStringProperty(), columns, firstRowExtractor, lastColumnExtractor != null ? firstRowLastColumnString : new SimpleStringProperty(), tableStyle)) : new SimpleObjectProperty<>();
	}

	protected ObservableList<Row> getElements(ObservableList<ITEM> items, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<T>> secondColumnExtractor, ObservableList<COL> columns,
			Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor, TableStyle tableStyle) {
		return new Transformation<Row, ITEM>(items, item -> getRowBuilder().build(item, firstColumnExtractor == null ? new SimpleStringProperty() : firstColumnExtractor.apply(item),
				secondColumnExtractor != null ? secondColumnExtractor.apply(item) : new SimpleObjectProperty<T>(), columns, col -> rowColumnExtractor.apply(item).apply(col),
				lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item), tableStyle));
	}

	// protected abstract RowBuilder<COL, T> getRowBuilder();
	//
	// protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefHeight();
	//
	// protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefWidth();
	//

	protected RowBuilder<COL, T> getRowBuilder() {
		return new TableCellRowBuilder();
	}

	protected <M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
		return app -> new SimpleObjectProperty<Number>(900);
		// return app -> ((GenericWindow) app).getWidth();
	}

	protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
		return app -> ((Window) app).getHeight();
	}

}