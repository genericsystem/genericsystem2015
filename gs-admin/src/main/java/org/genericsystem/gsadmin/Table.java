package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.layout.Pane;

import org.genericsystem.gsadmin.Row.ExtendedRow;
import org.genericsystem.gsadmin.Stylable.Listable;
import org.genericsystem.gsadmin.TableCreation.ExtendedTableCreation;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSVBox;

public class Table extends Listable<Row> {
	static {
		Func.put(Table.class, TableCreation.class, new TableCreation() {
		});
		Func.put(ExtendedTable.class, TableCreation.class, new ExtendedTableCreation() {
		});
	}

	public static void init(Element<?> parent) {
		GSVBox tablePanel = new GSVBox(parent, Pane::getChildren).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
		{
			new GSHBox(tablePanel).select(Table::getFirstElement).include(Row::init).setStyleClass(Row::getStyleClass);
			new GSHBox(tablePanel).forEach(Table::getElements).include(Row::init).setStyleClass(Row::getStyleClass);
		}
	}

	public Table(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
		super(tableStyle.table, firstRow, rows);
	}

	public static class ExtendedTable extends Table {

		public ExtendedTable(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
			super(firstRow, rows, tableStyle);
		}

		public static void init(Element<?> parent) {
			GSVBox tablePanel = new GSVBox(parent, Group::getChildren).setPrefWidth(800).setPrefHeight(600).setStyleClass(Table::getStyleClass);
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(Row::init).setStyleClass(Row::getStyleClass);
				new GSHBox(tablePanel).forEach(Table::getElements).include(ExtendedRow::init).setStyleClass(Row::getStyleClass);
			}
		}
	}
}
