package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.TableBuilder.TextTableBuilder;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Table;

public class GenericRow extends Row {
	private final StringProperty name = new SimpleStringProperty("");
	private final Generic item;

	public GenericRow(Generic item, ObservableValue<Cell<?>> secondCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell, ObservableValue<String> styleClass) {
		super(secondCell, cells, lastCell, styleClass);
		this.item = item;
	}

	public StringProperty getName() {
		return name;
	}

	public Generic getItem() {
		return item;
	}

	public void add() {
		item.addInstance(name.get());
	}

	public void delete() {
		item.remove();
	}

	public void selectRowEngineTable() {
		GenericCrud firstCrud = ((GenericWindow) (getParent().getParent()).getParent()).getFirstCrud().getValue();

		firstCrud.getTable().getValue().getSelectedRow().setValue(this);
		TableCellTableBuilder<Generic, Generic> tableModel = new TableCellTableBuilder<>(new ReadOnlyStringWrapper("Concretes"), new ReadOnlyStringWrapper("Action"), this.item.getObservableSubInstances(), this.item.getObservableAttributes().filtered(
				attribute -> attribute.isCompositeForInstances(this.item)), itemTableCell -> columnTableCell -> {
			TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null,
					null, firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
			Table tab = textTableModel.buildTable();
			return new ReadOnlyObjectWrapper<>(tab);

		}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), item -> {
			TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), FXCollections.observableArrayList(item),
					FXCollections.observableArrayList(item.getComponents()), item2 -> column -> new ReadOnlyStringWrapper("" + column), null, firstColumnString -> new ReadOnlyStringWrapper("" + firstColumnString), null);
			Table tab = textTableModel.buildTableFirstColumn();
			return new ReadOnlyObjectWrapper<>(tab);
		}, column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.buildTable();
		table.getFirstRowHeight().setValue(25);
		table.getFirstColumnWidth().setValue(200);
		table.getRowHeight().setValue(50);
		table.getColumnWidth().setValue(310);

		((GenericWindow) (getParent().getParent()).getParent()).getSecondCrud().setValue(new GenericCrud(new SimpleObjectProperty<>(table), this.item));
		createEditTable(firstCrud);
	}

	public void selectRowGenericTable() {
		createEditTable(((GenericWindow) (getParent().getParent().getParent().getParent().getParent().getParent())).getSecondCrud().getValue());
	}

	private void createEditTable(GenericCrud crud) {
		if (crud != null) {
			Generic generic = crud.getModel();
			TableCellTableBuilder<Generic, Generic> editTableModel = new TableCellTableBuilder<>(new ReadOnlyStringWrapper("Edition"), new ReadOnlyStringWrapper("Action"), generic.getObservableAttributes().filtered(
					attribute -> attribute.isCompositeForInstances(generic)), FXCollections.observableArrayList(this.item), itemTableCell -> columnTableCell -> {
				TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), this.item.getObservableHolders(itemTableCell), FXCollections.observableArrayList(), null,
								null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
				Table tab = textTableModel.buildTable();
				return new ReadOnlyObjectWrapper<>(tab);
			}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), itemTableCell -> {
				TextTableBuilder<Generic, Generic> textTableModel = new TextTableBuilder<>(new ReadOnlyStringWrapper("Table"), new ReadOnlyStringWrapper("Action"), FXCollections.observableArrayList(itemTableCell),
						FXCollections.observableArrayList(itemTableCell.getComponents()), null, null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
				Table tab = textTableModel.buildTableFirstColumn();
				return new ReadOnlyObjectWrapper<>(tab);
			}, null);
			Table editTable = editTableModel.buildTable();
			editTable.getFirstColumnWidth().setValue(200);
			editTable.getColumnWidth().setValue(310);
			editTable.getRowHeight().setValue(45);
			crud.getEditTable().setValue(editTable);
		}
	}
}
