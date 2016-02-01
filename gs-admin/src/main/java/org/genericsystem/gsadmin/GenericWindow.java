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
import org.genericsystem.gsadmin.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.TableBuilder.TextTableBuilder;
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

	public void cancel() {
		engineCrud.getValue().<Generic> getModel().getCurrentCache().clear();
	}

	public void mount() {
		engineCrud.getValue().<Generic> getModel().getCurrentCache().mount();
	}

	public StringBinding getCacheLevel() {
		return Bindings.createStringBinding(() -> "Cache level : " + engineCrud.getValue().<CocClientEngine> getModel().getCurrentCache().getCacheLevelObservable().getValue(), engineCrud.getValue().<CocClientEngine> getModel().getCurrentCache()
				.getCacheLevelObservable());
	}

	public void unmount() {
		engineCrud.getValue().<CocClientEngine> getModel().getCurrentCache().unmount();
	}

	public static GenericWindow createWindow(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {
		TableCellTableBuilder<Generic, Generic> tableModel = new TableCellTableBuilder<>(new ReadOnlyStringWrapper("Structurals"), new ReadOnlyStringWrapper("Action"), engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(
				attribute -> attribute.isCompositeForInstances(engine)), itemTableCell -> columnTableCell -> {
			TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), itemTableCell.getObservableHolders(columnTableCell),
							FXCollections.observableArrayList(itemTableCell.getComponents()), null, null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
			Table tab = textTableModel.buildTable();
			return new ReadOnlyObjectWrapper<>(tab);
		}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), itemTableCell -> {
			TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), FXCollections.observableArrayList(itemTableCell),
							FXCollections.observableArrayList(itemTableCell.getComponents()), item -> col -> new ReadOnlyStringWrapper("" + col), null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
			Table tab = textTableModel.buildTableFirstColumn();
			return new ReadOnlyObjectWrapper<>(tab);
		}, column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.buildTable();
		table.getFirstRowHeight().setValue(30);
		table.getFirstColumnWidth().setValue(100);
		table.getRowHeight().setValue(50);
		table.getColumnWidth().setValue(100);
		GenericCrud crud = new GenericCrud(new SimpleObjectProperty<>(table), engine);
		return new GenericWindow(crud, width, height);
	}
}
