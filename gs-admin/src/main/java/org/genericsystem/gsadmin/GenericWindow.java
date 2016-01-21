package org.genericsystem.gsadmin;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.StringBinding;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.TableBuilder.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilder.TextTableModel;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.Window;

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

	public StringBinding getCacheLevel() {
		return Bindings.createStringBinding(() -> "Cache level : " + tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().getCacheLevelObservable().getValue(), tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache()
				.getCacheLevelObservable());
	}

	public void unmount() {
		tableCrud.getValue().<CocClientEngine> getModel().getCurrentCache().unmount();
	}

	public void selectRowEngineTable(GenericRow row) {
		tableCrud.getValue().getTable().getValue().getSelectedRow().setValue(row);
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(row.getItem().getObservableSubInstances(), row.getItem().getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(row.getItem())),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, firstColumnString -> new ReadOnlyStringWrapper(""
							+ firstColumnString), null);
					Table tab = textTableModel.buildTable();
					tab.getColumnWidth().setValue(300);
					tab.getRowHeight().setValue(30);
					return new ReadOnlyObjectWrapper<>(tab);
				}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), item -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(item), FXCollections.observableArrayList(item.getComponents()), item2 -> column -> new ReadOnlyStringWrapper("" + column), null,
							firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
					Table tab = textTableModel.buildTable();
					tab.getColumnWidth().setValue(100);
					tab.getRowHeight().setValue(50);
					return new ReadOnlyObjectWrapper<>(tab);
				}, column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.buildTable();
		table.getFirstRowHeight().setValue(25);
		table.getFirstColumnWidth().setValue(200);
		table.getRowHeight().setValue(50);
		table.getColumnWidth().setValue(310);
		tableCrudSelectedRow.setValue(new GenericCrud(new SimpleObjectProperty<>(table), row.getItem()));
		createEditTable(tableCrud, row);
	}

	public void selectRowGenericTable(GenericRow row) {
		assert row != null;
		createEditTable(tableCrudSelectedRow, row);
	}

	private void createEditTable(Property<GenericCrud> crud, GenericRow row) {
		if (crud.getValue() != null) {
			Generic generic = crud.getValue().getModel();
			TableCellTableModel<Generic, Generic> editTableModel = new TableCellTableModel<>(generic.getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generic)), FXCollections.observableArrayList(row.getItem()),
					itemTableCell -> columnTableCell -> {
						TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(row.getItem().getObservableHolders(itemTableCell), FXCollections.observableArrayList(), null, null, firstColumString -> new ReadOnlyStringWrapper(""
								+ firstColumString), null);
						Table tab = textTableModel.buildTable();
						return new ReadOnlyObjectWrapper<>(tab);
					}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), itemTableCell -> {
						TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(itemTableCell), FXCollections.observableArrayList(itemTableCell.getComponents()), null, null,
								firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
						Table tab = textTableModel.buildTable();
						tab.getFirstColumnWidth().setValue(100);

						tab.getFirstRowHeight().setValue(30);
						tab.getRowHeight().setValue(40);

						return new ReadOnlyObjectWrapper<>(tab);
					}, null);
			Table editTable = editTableModel.buildTable();
			editTable.getFirstColumnWidth().setValue(200);
			editTable.getColumnWidth().setValue(310);
			editTable.getRowHeight().setValue(45);
			crud.getValue().getEditTable().setValue(editTable);
		}
	}

	public static GenericWindow createWindow(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(engine)),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(itemTableCell.getComponents()), null, null,
							firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
					Table tab = textTableModel.buildTable();
					return new ReadOnlyObjectWrapper<>(tab);
				}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), itemTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(itemTableCell), FXCollections.observableArrayList(itemTableCell.getComponents()), item -> col -> new ReadOnlyStringWrapper(""
							+ col), null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
					Table tab = textTableModel.buildTable();
					tab.getFirstColumnWidth().setValue(150);

					tab.getFirstRowHeight().setValue(30);
					tab.getRowHeight().setValue(75);

					return new ReadOnlyObjectWrapper<>(tab);
				}, column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.buildTable();
		table.getFirstRowHeight().setValue(30);
		table.getFirstColumnWidth().setValue(300);
		table.getRowHeight().setValue(80);
		table.getColumnWidth().setValue(310);

		GenericCrud crud = new GenericCrud(new SimpleObjectProperty<>(table), engine);
		return new GenericWindow(crud, width, height);
	}
}
