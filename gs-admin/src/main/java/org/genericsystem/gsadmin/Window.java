package org.genericsystem.gsadmin;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

public class Window {
	
	private Property<Table> table = new SimpleObjectProperty<>();
	
	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;
	private Property<Table> tableSelectedRow = new SimpleObjectProperty<>();
	private final CocClientEngine engine;
	
	public Window(CocClientEngine engine,Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.table = table;
		this.engine = engine;
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;
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
	
	public void flush(){
			engine.getCurrentCache().flush();
	}
	
	public void cancel(){
		engine.getCurrentCache().clear();
	}
	
	public void mount(){
		engine.getCurrentCache().mount();
	}
	
	public void unmount(){
		engine.getCurrentCache().unmount();
	}
	
	public Property<Table> getTableSelectedRow() {
		return tableSelectedRow;
	}
	
	public void selectRow(Row row){
		table.getValue().getSelectedRow().setValue(row);
		
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(((Generic)row.getItem()).getObservableSubInstances(), ((Generic)row.getItem()).getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances((Generic)row.getItem())), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, column -> new ReadOnlyStringWrapper("" + column));
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, column -> new ReadOnlyStringWrapper("" + column), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString));

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(120);
		table.getRowHeight().setValue(20);
		table.getFirstRowHeight().setValue(20);
		tableSelectedRow.setValue(table);
	}
	
}
