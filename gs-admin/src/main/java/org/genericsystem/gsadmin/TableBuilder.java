package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.RowBuilder.TableCellRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextCellFirstRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextCellRowBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilder<ITEM, COL, T> implements Builder {

	protected Table build(ObservableList<ITEM> items, ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, Function<ITEM, ObservableValue<String>> rowfirstColumnString,
			Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, TableStyle tableStyle) {
		return new Table(getFirstElement(firstColumnString, columns, columnExtractor, tableStyle), getElements(items, rowfirstColumnString, columns, rowColumnExtractor, tableStyle), getStyle(tableStyle));
	}

	protected ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.table;
	}

	protected ObservableValue<Row> getFirstElement(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle) {
		return new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(firstColumnString, columns, columnExtractor, tableStyle));
	}

	protected ObservableList<Row> getElements(ObservableList<ITEM> items, Function<ITEM, ObservableValue<String>> rowfirstColumnString, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, TableStyle tableStyle) {
		return new Transformation<Row, ITEM>(items, item -> getRowBuilder().build(rowfirstColumnString.apply(item), columns, col -> rowColumnExtractor.apply(item).apply(col), tableStyle));
	}

	@Override
	public void init(Element<?> parent) {
		GSSCrollPane scrollPane = new GSSCrollPane(parent).setStyleClass("scrollable");
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setMinWidth(Table::getTableWidth).setSuperPrefWidth(getSuperPrefWidth()).setMinHeight(Table::getTableHeight).setSuperPrefHeight(getSuperPrefHeight());
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass).setMinHeight(Table::getFirstRowHeight).setMaxHeight(Table::getFirstRowHeight)
						.setPrefHeight(Table::getFirstRowHeight);
				new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass).setMinHeight(Table::getRowHeight).setMaxHeight(Table::getRowHeight).setPrefHeight(Table::getRowHeight);
			}
		}
	}

	abstract RowBuilder<COL, T> getRowBuilder();

	abstract <M> Function<M, ObservableValue<Number>> getSuperPrefHeight();

	abstract <M> Function<M, ObservableValue<Number>> getSuperPrefWidth();

	public static class TextCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, String> {

		@Override
		RowBuilder<COL, String> getRowBuilder() {
			return new TextCellRowBuilder<COL>();
		}

		@Override
		<M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
			return table -> ((Table) table).getColumnWidth();
		}

		@Override
		<M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			return table -> ((Table) table).getRowHeight();
		}

	}

	public static class TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, Table> {

		@Override
		RowBuilder<COL, Table> getRowBuilder() {
			return new TableCellRowBuilder<COL>();
		}

		@Override
		<M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
			return app -> ((Window) app).getWidth();
		}

		@Override
		<M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			return app -> ((Window) app).getHeight();
		}
	}
}