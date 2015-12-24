package org.genericsystem.gsadmin;

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

public abstract class TableBuilder<ITEM, COL, T> extends ElementBuilder {
	Table build(TableModel<ITEM, COL, T> tableModel) {
		return new Table(getFirstElement(tableModel), getElements(tableModel), tableModel.getTableStyle());
	}

	private ObservableValue<Row> getFirstElement(TableModel<ITEM, COL, T> tableModel) {
		return new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(tableModel.getFirstRowFirstColumnString(), tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle()));
	}

	private ObservableList<Row> getElements(TableModel<ITEM, COL, T> tableModel) {
		return new Transformation<Row, ITEM>(tableModel.getItems(), item -> getRowBuilder().build(tableModel.getRowfirstColumnString() != null ? tableModel.getRowfirstColumnString().apply(item) : null, tableModel.getColumns(),
				tableModel.getRowColumnExtractor().apply(item), tableModel.getTableStyle()));
	}

	@Override
	public void init(Element<?> parent) {
		GSSCrollPane scrollPane = new GSSCrollPane(parent);
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass);
				new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass);
			}
		}
	}

	abstract RowBuilder<T, COL> getRowBuilder();

	public static class TextCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, String> {
		@Override
		RowBuilder<String, COL> getRowBuilder() {
			return new TextCellRowBuilder<COL>();
		}
	}

	public static class TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, Table> {
		@Override
		RowBuilder<Table, COL> getRowBuilder() {
			return new TableCellRowBuilder<COL>();
		}
	}
}