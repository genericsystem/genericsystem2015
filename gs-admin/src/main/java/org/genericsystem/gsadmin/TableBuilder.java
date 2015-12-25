package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Builder.ElementBuilder;
import org.genericsystem.gsadmin.RowBuilder.TableCellRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextCellFirstRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextCellRowBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilder<ITEM, COL, U, T> extends ElementBuilder {
	Table build(TableModel<ITEM, COL, U, T> tableModel) {
		return new Table(getFirstElement(tableModel), getElements(tableModel), tableModel.getTableStyle());
	}

	private ObservableValue<Row> getFirstElement(TableModel<ITEM, COL, U, T> tableModel) {

		RowMetaModel<COL, U, String> rowMetaModel = new RowMetaModel<COL, U, String>(tableModel.getFirstRowFirstColumnString(), tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle());

		return new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL, U>().build(rowMetaModel));
		// return new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(tableModel.getFirstRowFirstColumnString(), tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle()));
	}

	private ObservableList<Row> getElements(TableModel<ITEM, COL, U, T> tableModel) {
		return new Transformation<Row, ITEM>(tableModel.getItems(), item -> {
			Function<COL, ObservableValue<T>> apply = tableModel.getRowColumnExtractor().apply(item);
			Function<COL, ObservableValue<U>> result = col -> {
				ObservableValue<T> apply2 = apply.apply(col);
				assert apply2.getValue() != null;
				ObservableValue<U> result2 = Bindings.<U> createObjectBinding(() -> (U) apply2.getValue(), apply2);
				return result2;
			};
			return getRowBuilder().build(new RowMetaModel<>(tableModel.getRowfirstColumnString().apply(item), tableModel.getColumns(), result, tableModel.getTableStyle()));
		});
	}

	@Override
	public void init(Element<?> parent) {
		GSSCrollPane scrollPane = new GSSCrollPane(parent);
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass);
				new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass);
				;
			}
		}
	}

	abstract RowBuilder<COL, U, T> getRowBuilder();

	public static class TextCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, String, String> {

		@Override
		RowBuilder<COL, String, String> getRowBuilder() {
			return new TextCellRowBuilder<COL, String>();
		}
	}

	public static class TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, String, Table> {
		@Override
		RowBuilder<COL, String, Table> getRowBuilder() {
			return new TableCellRowBuilder<COL, String>();
		}
	}
}