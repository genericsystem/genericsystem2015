package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.layout.Pane;

import org.genericsystem.gsadmin.RowBuilder.TableCellRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextCellFirstRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextCellRowBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilder<ITEM, COL, T> implements Builder {
	Table build(TableModel<ITEM, COL, T> tableModel) {
		ObservableValue<Row> firstRow = new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(tableModel.getFirstRowFirstColumnString(), tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle()));
		ObservableList<Row> rows = new Transformation<Row, ITEM>(tableModel.getItems(), item -> getRowBuilder().build(tableModel.getRowfirstColumnString() != null ? tableModel.getRowfirstColumnString().apply(item) : null, tableModel.getColumns(),
				tableModel.getRowColumnExtractor().apply(item), tableModel.getTableStyle()));
		return new Table(firstRow, rows, tableModel.getTableStyle());
	}

	@Override
	public void init(Element<?> parent) {
		GSVBox tablePanel = new GSVBox(parent, Group::getChildren).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
		{
			new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass);
			new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass);
		}
	}

	abstract RowBuilder<T, COL> getRowBuilder();

	public static class TextCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, String> {
		@Override
		public void init(Element<?> parent) {
			GSVBox tablePanel = new GSVBox(parent, Pane::getChildren).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass);
				new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass);
			}
		}

		@Override
		RowBuilder<String, COL> getRowBuilder() {
			return new TextCellRowBuilder<COL>();
		}
	}

	public static class TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, Table> {
		@Override
		public void init(Element<?> parent) {
			GSVBox tablePanel = new GSVBox(parent, Group::getChildren).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass);
				new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(Row::getStyleClass);
			}
		}

		@Override
		RowBuilder<Table, COL> getRowBuilder() {
			return new TableCellRowBuilder<COL>();
		}
	}
}