package org.genericsystem.gsadmin;

import java.text.SimpleDateFormat;
import java.util.Date;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.StringBinding;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.ClientEngine;
import org.genericsystem.gsadmin.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.TableBuilder.TextTableBuilder;
import org.genericsystem.kernel.Statics;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.Window;

public class GenericWindow extends Window {

	private final Property<GenericCrud> engineCrud = new SimpleObjectProperty<>();
	private final Property<GenericCrud> genericCrud = new SimpleObjectProperty<>();

	public GenericWindow(GenericCrud tableCrud, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		super(width, height);
		this.engineCrud.setValue(tableCrud);
	}

	public Property<GenericCrud> getFirstCrud() {
		return engineCrud;
	}

	public Property<GenericCrud> getSecondCrud() {
		return genericCrud;
	}

	public void flush() {
		engineCrud.getValue().<Generic> getModel().getCurrentCache().flush();
	}

	public void shiftTs() {
		engineCrud.getValue().<Generic> getModel().getCurrentCache().shiftTs();
	}

	public StringBinding getTs() {
		return Bindings.createStringBinding(() -> "TS : " + new SimpleDateFormat("dd:MM:YY / HH:mm:ss").format(new Date(engineCrud.getValue().<ClientEngine> getModel().getCurrentCache().getTransaction().getTs() / Statics.MILLI_TO_NANOSECONDS)),
				engineCrud.getValue().<ClientEngine> getModel().getCurrentCache().getObservableTransaction());
	}

	public void cancel() {
		engineCrud.getValue().<Generic> getModel().getCurrentCache().clear();
	}

	public void mount() {
		engineCrud.getValue().<Generic> getModel().getCurrentCache().mount();
	}

	public StringBinding getCacheLevel() {
		return Bindings.createStringBinding(() -> "Cache level : " + engineCrud.getValue().<ClientEngine> getModel().getCurrentCache().getCacheLevelObservable().getValue(), engineCrud.getValue().<ClientEngine> getModel().getCurrentCache()
				.getCacheLevelObservable());
	}

	public void unmount() {
		engineCrud.getValue().<ClientEngine> getModel().getCurrentCache().unmount();
	}

	static ReadOnlyObjectWrapper<Table> createCellContent(Generic itemTableCell,Generic columnTableCell){
			TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), itemTableCell.getObservableHolders(columnTableCell),
					FXCollections.observableArrayList(itemTableCell.getComponents()), null, null, currentGeneric -> new ReadOnlyStringWrapper("" + currentGeneric), null);
			Table tab = textTableModel.buildTable(0, 0);
			return new ReadOnlyObjectWrapper<>(tab);
	}
	
	static ReadOnlyObjectWrapper<Table> createFirstColumnTable(Generic item){
		TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), FXCollections.observableArrayList(item),
				FXCollections.observableArrayList(item.getComponents()), item2 -> col2 -> new ReadOnlyStringWrapper("" + col2), null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
		Table tab = textTableModel.buildTableFirstColumn();
		return new ReadOnlyObjectWrapper<>(tab);
	
	}
	
	public static GenericWindow createWindow(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, ClientEngine engine) {
		TableCellTableBuilder<Generic, Generic> tableModel = new TableCellTableBuilder<>(new ReadOnlyStringWrapper("Structurals"), new ReadOnlyStringWrapper("Action"), engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(
				attribute -> attribute.isCompositeForInstances(engine)),item->col->createCellContent(item,col) , firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), itemTableCell -> createFirstColumnTable(itemTableCell), column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.buildTable(900, 400);
		table.getFirstRowHeight().setValue(30);
		table.getFirstColumnWidth().setValue(207);
		table.getRowHeight().setValue(50);
		table.getColumnWidth().setValue(94);
		GenericCrud crud = new GenericCrud(new SimpleObjectProperty<>(table), engine);
		return new GenericWindow(crud, width, height);
	}
}
