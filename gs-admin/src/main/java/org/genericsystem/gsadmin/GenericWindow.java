package org.genericsystem.gsadmin;

import java.util.Arrays;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.Window;

import com.sun.javafx.collections.ObservableListWrapper;

@SuppressWarnings("restriction")
public class GenericWindow extends Window{
	private Property<Table> table = new SimpleObjectProperty<>();
	private Property<Table> tableSelectedRow = new SimpleObjectProperty<>();
	private Property<Table> editTableSelectedRow = new SimpleObjectProperty<>();
	private final CocClientEngine engine;
	private StringProperty name = new SimpleStringProperty();
	
	public GenericWindow(CocClientEngine engine,Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		super(width,height);
		this.table = table;
		this.engine = engine;
	}

	public ObservableValue<Table> getTable() {
		return table;
	}
	
	public StringProperty getName() {
		return name;
	}
	
	public void add(){
		engine.addInstance(name.getValue());
}
	
	public void flush(){
			engine.getCurrentCache().flush();
	}
	
	public void shiftTs(){
		engine.getCurrentCache().shiftTs();
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
	
	public Property<Table> getEditTableSelectedRow() {
		return editTableSelectedRow;
	}

	public void selectRow(GenericRow row){		
		table.getValue().getSelectedRow().setValue(row);
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(row.getItem().getObservableSubInstances(), row.getItem().getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(row.getItem())), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);

//		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(((Generic)row.getItem()).getObservableSubInstances(), ((Generic)row.getItem()).getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances((Generic)row.getItem())), itemTableCell -> columnTableCell -> {
//			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), item2 -> column -> new ReadOnlyStringWrapper("" +  item2), firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString),null);

			Table tab = textTableModel.createTable();
			tab.getColumnWidth().setValue(300);
			return new ReadOnlyObjectWrapper<Table>(tab);

		}, column -> new ReadOnlyStringWrapper("" + column), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(100);
		table.getFirstRowHeight().setValue(20);
		tableSelectedRow.setValue(table);


		// Edit table model
		TableCellTableModel<Generic, Generic> editTableModel = new TableCellTableModel<>(row.getItem().getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(row.getItem())), new ObservableListWrapper<>(
				Arrays.asList(row.getItem())), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, column -> new ReadOnlyStringWrapper("" + column), null);
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
		Table editTable = editTableModel.createTable();
		editTable.getColumnWidth().setValue(120);
		editTable.getRowHeight().setValue(20);
		editTable.getFirstRowHeight().setValue(20);
		editTableSelectedRow.setValue(editTable);
	}
	
}
