package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
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

	public Window build(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height,CocClientEngine engine) {
		
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>( FXCollections.observableArrayList(engine.getSubInstances().toList()),FXCollections.observableArrayList(engine.getAttributes().toList()),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(itemTableCell.getHolders(columnTableCell).toList()), FXCollections.observableArrayList(), 
							null,null,column -> new ReadOnlyStringWrapper("" + column));
					Table tab = textTableModel.createTable();
					tab.getFirstColumnWidth().setValue(295);
					tab.getRowHeight().setValue(100);
					return new ReadOnlyObjectWrapper<Table>(tab);
				}
				,	column -> new ReadOnlyStringWrapper("" + column),
					firstColumString->new ReadOnlyStringWrapper(""+firstColumString));
		
		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(100);
		table.getFirstRowHeight().setValue(50);
		table.getFirstColumnWidth().setValue(200);
		return new Window(new ReadOnlyObjectWrapper<Table>(table), width, height);
	}
}
