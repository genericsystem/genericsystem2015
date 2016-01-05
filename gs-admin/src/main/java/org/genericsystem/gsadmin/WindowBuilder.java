package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.gsadmin.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSVBox;
import javafx.beans.property.ReadOnlyStringWrapper;

public class WindowBuilder implements Builder {

	@Override
	public void init(Element<?> parent) {
		GSVBox table = new GSVBox(parent).select(Window::getTable);
		{
			new TableCellTableBuilder<>().init(table);
		}
	}

	public Window build(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		
		TableCellTableModel<Integer, Integer> tableModel = new TableCellTableModel<>(FXCollections.observableArrayList(0, 1, 2, 3), FXCollections.observableArrayList(0, 1, 2),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Integer, Integer> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(5, 8, 8, 9), FXCollections.observableArrayList(1, 2, 7, 6), 
							itemTextTable -> columnTextTable -> new ReadOnlyStringWrapper("Cell : " + itemTextTable + " " + columnTextTable),null,null);
					return new ReadOnlyObjectWrapper<Table>(textTableModel.createTable());
				},	column -> new ReadOnlyStringWrapper("Column : " + column),
					firstColumString->new ReadOnlyStringWrapper(""+firstColumString));
		
		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(100);
		table.getFirstRowHeight().setValue(50);
		return new Window(new ReadOnlyObjectWrapper<Table>(table), width, height);
	}
}
