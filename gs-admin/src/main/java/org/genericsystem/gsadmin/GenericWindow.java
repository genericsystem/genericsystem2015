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
import org.genericsystem.gsadmin.Crud.*;
import com.sun.javafx.collections.ObservableListWrapper;

@SuppressWarnings("restriction")
public class GenericWindow extends Window{
//	private Property<Table> table = new SimpleObjectProperty<>();
//	private Property<Table> tableSelectedRow = new SimpleObjectProperty<>();
//	private Property<Table> editTableSelectedRow = new SimpleObjectProperty<>();
//	private CocClientEngine engine;
//	private StringProperty name = new SimpleStringProperty();
	
	
	private Property<Crud> tableCrud = new SimpleObjectProperty<>();
	private Property<Crud> tableCrudSelectedRow = new SimpleObjectProperty<>();
	private Property<Crud> editTableCrudSelectedRow = new SimpleObjectProperty<>();
	
	public Property<Crud> getEditTableCrudSelectedRow() {
		return editTableCrudSelectedRow;
	}
	
	public Property<Crud> getTableCrud() {
		return tableCrud;
	}
	
	public Property<Crud> getTableCrudSelectedRow() {
		return tableCrudSelectedRow;
	}
	
	public GenericWindow(CocClientEngine engine,Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		super(width,height);
//		this.table = table;
//		this.engine = engine;
	}


	public GenericWindow(Crud tableCrud, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		super(width,height);
		this.tableCrud.setValue(tableCrud);
	}
	
//	public ObservableValue<Table> getTable() {
//		return table;
//	}
//	
//	public StringProperty getName() {
//		return name;
//	}
//	
//	public void add(){
//		engine.addInstance(name.getValue());
//	}
//	
	public void flush(){
		tableCrud.getValue().<CocClientEngine>getModel().getCurrentCache().flush();
	}
	
	public void shiftTs(){
		tableCrud.getValue().<CocClientEngine>getModel().getCurrentCache().shiftTs();
	}
	
	public void cancel(){
		tableCrud.getValue().<CocClientEngine>getModel().getCurrentCache().clear();
	}
	
	public void mount(){
		tableCrud.getValue().<CocClientEngine>getModel().getCurrentCache().mount();
	}
	
	public void unmount(){
		tableCrud.getValue().<CocClientEngine>getModel().getCurrentCache().unmount();
	}
//	
//	public Property<Table> getTableSelectedRow() {
//		return tableSelectedRow;
//	}
//	
//	public Property<Table> getEditTableSelectedRow() {
//		return editTableSelectedRow;
//	}

	public void selectRow(GenericRow row){		
		tableCrud.getValue().getTable().getValue().getSelectedRow().setValue(row);
		System.out.println("item :: ");
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(row.getItem().getObservableSubInstances(), row.getItem().getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(row.getItem())), itemTableCell -> columnTableCell -> {
			System.out.println(itemTableCell);
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(),
//					item->col->{
//						TextTableModel<Integer, Integer> textTableModel2 = new TextTableModel<>(FXCollections.observableArrayList(1,2,3,4), FXCollections.observableArrayList(0,0,0,0),
//								item2 -> column -> new ReadOnlyStringWrapper("Cell : " + item2 + " " + column), firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
//						return new ReadOnlyObjectWrapper<Table>(textTableModel2.createTable());
//					}

					item2 -> column -> new ReadOnlyStringWrapper("" + item2.getComponent(0)), null, firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
			Table tab = textTableModel.createTable();
			tab.getColumnWidth().setValue(300);
			return new ReadOnlyObjectWrapper<Table>(tab);

		}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(100);
		table.getFirstRowHeight().setValue(20);
//		tableSelectedRow.setValue(table);
		tableCrudSelectedRow.setValue(new GenericCrud(new SimpleObjectProperty<Table>(table),row.getItem()));


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
		//editTableSelectedRow.setValue(editTable);
		editTableCrudSelectedRow.setValue(new GenericCrud(new SimpleObjectProperty<Table>(table),row.getItem()));

	}
	
}
