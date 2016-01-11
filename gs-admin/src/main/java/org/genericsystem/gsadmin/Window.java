package org.genericsystem.gsadmin;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

public class Window {

	

	private Property<Table> table = new SimpleObjectProperty<>();
	private Property<Table> tableSelectedRow = new SimpleObjectProperty<>();
	private Property<Row> selectedRow = new SimpleObjectProperty<>();	
	
	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;
	
	public Window(Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.table = table;
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;
	}

	public Property<Row> getSelectedRow() {
		return selectedRow;
	}
	
	public ObservableValue<Table> getTableSelectedRow() {
		return tableSelectedRow;
	}
	
	public ObservableValue<Number> getWidth() {
		return width;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	public ObservableValue<Table> getTable() {
		return table;
	}
	
	public void selectRow(Row row) {
		selectedRow.setValue(row);
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(((Generic)row.getItem()).getObservableSubInstances(), ((Generic)row.getItem()).getObservableAttributes(), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, column -> new ReadOnlyStringWrapper("" + column));
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, column -> new ReadOnlyStringWrapper("" + column), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString));

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(400);
		table.getRowHeight().setValue(50);
		table.getFirstRowHeight().setValue(50);
		tableSelectedRow.setValue(table);
	}
}
