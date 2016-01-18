package org.genericsystem.gsadmin;

import java.util.Arrays;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
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
public class GenericWindow extends Window {

	private Property<GenericCrud> tableCrud = new SimpleObjectProperty<>();
	private Property<GenericCrud> tableCrudSelectedRow = new SimpleObjectProperty<>();

	public Property<GenericCrud> getTableCrud() {
		return tableCrud;
	}

	public Property<GenericCrud> getTableCrudSelectedRow() {
		return tableCrudSelectedRow;
	}

	public GenericWindow(CocClientEngine engine, Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		super(width, height);
	}

	public GenericWindow(GenericCrud tableCrud, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		super(width, height);
		this.tableCrud.setValue(tableCrud);
	}

	public void flush() {
		tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().flush();
	}

	public void shiftTs() {
		tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().shiftTs();
	}

	public void cancel() {
		tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().clear();
	}

	public void mount() {
		tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().mount();
	}

	public void unmount() {
		tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().unmount();
	}

	public void selectRowEngineTable(GenericRow row) {
		tableCrud.getValue().getTable().getValue().getSelectedRow().setValue(row);
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(row.getItem().getObservableSubInstances(), row.getItem().getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(row.getItem())),
				itemTableCell -> columnTableCell -> {
					System.out.println(itemTableCell);
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(),
					// item->col->{
					// TextTableModel<Integer, Integer> textTableModel2 = new TextTableModel<>(FXCollections.observableArrayList(1,2,3,4), FXCollections.observableArrayList(0,0,0,0),
					// item2 -> column -> new ReadOnlyStringWrapper("Cell : " + item2 + " " + column), firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
					// return new ReadOnlyObjectWrapper<Table>(textTableModel2.createTable());
					// }
							item2 -> column -> new ReadOnlyStringWrapper("" + item2.getComponent(0)), null, firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
					Table tab = textTableModel.createTable();
					tab.getColumnWidth().setValue(300);
					return new ReadOnlyObjectWrapper<Table>(tab);

				}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.createTable();
		table.getFirstRowHeight().setValue(25);
		table.getFirstColumnWidth().setValue(200);
		table.getRowHeight().setValue(22);
		table.getColumnWidth().setValue(310);
		tableCrudSelectedRow.setValue(new GenericCrud(new SimpleObjectProperty<Table>(table), row.getItem()));
		CocClientEngine engine = tableCrud.getValue().<CocClientEngine> getModel();
		createEditTable(tableCrud, row, engine);
	}

	public void selectRowGenericTable(GenericRow row) {
		createEditTable(tableCrudSelectedRow, row, row.getItem());
	}

	private void createEditTable(Property<GenericCrud> crud, GenericRow row, Generic generic) {
		TableCellTableModel<Generic, Generic> editTableModel = new TableCellTableModel<>(generic.getObservableAttributes()/* .filtered(attribute -> attribute.isCompositeForInstances(engine)) */, new ObservableListWrapper<>(Arrays.asList(row.getItem())),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(columnTableCell.getObservableHolders(itemTableCell), FXCollections.observableArrayList(columnTableCell.getComponents()),
							item2 -> column -> new ReadOnlyStringWrapper("" + item2), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
					Table tab = textTableModel.createTable();
					return new ReadOnlyObjectWrapper<Table>(tab);
				}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
		Table editTable = editTableModel.createTable();
		editTable.getFirstColumnWidth().setValue(200);
		editTable.getColumnWidth().setValue(310);
		editTable.getRowHeight().setValue(45);
		crud.getValue().getEditTable().setValue(editTable);
	}
}
