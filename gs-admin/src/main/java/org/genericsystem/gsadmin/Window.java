package org.genericsystem.gsadmin;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

public class Window {

	

	private Property<Table> table = new SimpleObjectProperty<>();
	private Property<Table> tableSelectedRow = new SimpleObjectProperty<>();
	
	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;
	
	private ObservableNumberValue mainPanelHeight = Bindings.createIntegerBinding(()-> computeHeight().getValue().intValue(), getTable(),getTableSelectedRow());
	private ObservableNumberValue tableSelectedRowExist =Bindings.createIntegerBinding(() -> tableSelectedRow.getValue() != null ? 1 : 0, tableSelectedRow);
	
	private ObservableNumberValue computeHeight() {
		ObservableNumberValue v = Bindings.add(table.getValue().getTableHeight(),tableSelectedRowExist.getValue().intValue()!=0?tableSelectedRow.getValue().getTableHeight():tableSelectedRowExist);
		System.out.println(v.intValue());
		return v;
	}
	
	public Window(Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.table = table;
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;
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
	
	public ObservableValue<Number> getmainPanelHeight() {
		return mainPanelHeight;
	}
	
	public void selectRow(Row row) {
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(((Generic)row.getItem()).getObservableSubInstances(), ((Generic)row.getItem()).getObservableAttributes(), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, column -> new ReadOnlyStringWrapper("" + column));
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, column -> new ReadOnlyStringWrapper("" + column), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString));

//		TableCellTableModel<Integer, Integer> tableModel = new TableCellTableModel<>(FXCollections.observableArrayList(0, 1, 2, 3), FXCollections.observableArrayList(0, 1, 2), item -> col -> {
//			TextTableModel<Integer, Integer> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(5, 8, 9, 6), FXCollections.observableArrayList(1, 1, 1, 1),
//					item2 -> column -> new ReadOnlyStringWrapper("Cell : " + item2 + " " + column), null, null);
//			Table tab = textTableModel.createTable();
//			return new ReadOnlyObjectWrapper<Table>(tab);
//		}, col -> new ReadOnlyStringWrapper("col :" + col), item -> new ReadOnlyStringWrapper("item :" + item));

		
		Table table = tableModel.createTable();
		tableSelectedRow.setValue(table);
	}
}
