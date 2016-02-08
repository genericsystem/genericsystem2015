package org.genericsystem.gsadmin;

import java.text.SimpleDateFormat;
import java.util.Date;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.StringBinding;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.experimental.Builders;
import org.genericsystem.experimental.Builders.CellBuilder;
import org.genericsystem.experimental.Builders.RowBuilder;
import org.genericsystem.experimental.Builders.TableBuilder;
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
		return Bindings.createStringBinding(() -> "TS : " + new SimpleDateFormat("dd:MM:YY / HH:mm:ss").format(new Date(engineCrud.getValue().<CocClientEngine> getModel().getCurrentCache().getTransaction().getTs() / Statics.MILLI_TO_NANOSECONDS)),
				engineCrud.getValue().<CocClientEngine> getModel().getCurrentCache().getObservableTransaction());
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

	// ***********************************************************************************************************************
	// renvoie le contenu de chaque cellule du tableau "tableModel"
	private static ReadOnlyObjectWrapper<Table> createInnerTableEngine(Generic itemTableCell, Generic columnTableCell) {

		// TableBuilder<Generic, Generic, String> textTableModel = new TableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), itemTableCell.getObservableHolders(columnTableCell),
		// FXCollections.observableArrayList(itemTableCell.getComponents()), item -> col -> new ReadOnlyStringWrapper("testtt"), null, firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
		// Table tab = textTableModel.buildTable(0, 0);
		// return new ReadOnlyObjectWrapper<>(tab);
		return null;

	}

	// renvoie le contenu de chaque cellule de la première colonne
	private static ReadOnlyObjectWrapper<Table> createFirstColumnTableCell(Generic itemTableCell) {
		//
		// TableBuilder<Generic, Generic, String> textTableModel = new TableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), FXCollections.observableArrayList(itemTableCell), FXCollections.observableArrayList(itemTableCell
		// .getComponents()), item -> col -> new ReadOnlyStringWrapper("" + col), null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
		// Table tab = textTableModel.buildTableFirstColumn();
		// return new ReadOnlyObjectWrapper<>(tab);
		return null;

	}

	public static GenericWindow createWindow(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {

		Builders.TableBuilder<Generic, Generic> tableBuilder = new Builders.TableBuilder<>(new ReadOnlyStringWrapper("Structurals"), new ReadOnlyStringWrapper("Action"), engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(
				attribute -> attribute.isCompositeForInstances(engine)));
		Builders.RowBuilder<Generic> rowBuilder = new Builders.RowBuilder<>(tableBuilder);
		Builders.CellBuilder<Generic, Generic, Table> cellBuilder = new CellBuilder<>(rowBuilder, item -> col -> {

			TableBuilder<Generic, Generic> innerTableBuilder = new TableBuilder<>(new ReadOnlyStringWrapper("Structurals"), new ReadOnlyStringWrapper("Structurals"), FXCollections.observableArrayList(item), FXCollections.observableArrayList(col));
			RowBuilder<Generic> innerRowBuilder = new RowBuilder<>(innerTableBuilder);
			Builders.CellBuilder<Generic, Generic, String> innerCellBuilder = new Builders.CellBuilder<>(innerRowBuilder, i -> c -> {
				return new ReadOnlyStringWrapper("Structurals");
			});

			ObservableList listCell = innerCellBuilder.buildCell(new SimpleStringProperty("z"));
			ObservableList listRow = innerRowBuilder.buildRow(new SimpleStringProperty("z"), null, listCell, null);
			ObservableValue<Table> innerTable = innerTableBuilder.buildTable(200, 200, new SimpleStringProperty(""), null, listRow);

			return innerTable;
		});

		ObservableList listCell = cellBuilder.buildCell(new SimpleStringProperty(""));
		ObservableList listRow = rowBuilder.buildRow(new SimpleStringProperty(""), null, listCell, null);
		Table table = tableBuilder.buildTable(500, 500, new SimpleStringProperty(""), null, listRow).getValue();

		// ObservableList<Cell> cells = Builders.buildCell(rows,cellExtractor)

		// TableBuilder<Generic, Generic, Table> tableModel = new TableBuilder<>(new ReadOnlyStringWrapper("Structurals"), new ReadOnlyStringWrapper("Action"), engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(
		// attribute -> attribute.isCompositeForInstances(engine)), item -> column -> createInnerTableEngine(item, column), firstRowString -> new ReadOnlyStringWrapper("" + firstRowString),
		// itemFirstColumnTableCell -> createFirstColumnTableCell(itemFirstColumnTableCell), column -> new ReadOnlyStringWrapper("Delete"));

		// Table table = tableModel.buildTable(900, 400);
		// table.getFirstRowHeight().setValue(30);
		// table.getFirstColumnWidth().setValue(207);
		// table.getRowHeight().setValue(50);
		// table.getColumnWidth().setValue(94);
		GenericCrud crud = new GenericCrud(new SimpleObjectProperty<>(table), engine);
		return new GenericWindow(crud, width, height);

	}
}
