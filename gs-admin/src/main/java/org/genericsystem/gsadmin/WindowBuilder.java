package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.Crud.GenericCrud;
import org.genericsystem.gsadmin.GenericCrudBuilders.EngineCrudBuilder;
import org.genericsystem.gsadmin.GenericCrudBuilders.GenericCrudBuilder;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Builder;
import org.genericsystem.ui.table.Table;

public class WindowBuilder implements Builder {
	@Override
	public void init(Element<?> parent) {
		GSVBox mainPanel = new GSVBox(parent).setPrefHeight(GenericWindow::getHeight);
		{
			GSSCrollPane scrollPane = new GSSCrollPane(mainPanel).setStyleClass("scrollable");
			{

				GSVBox leftTables = new GSVBox(scrollPane).setMinHeight(500);
				{

					GSVBox crud = new GSVBox(leftTables).select(GenericWindow::getTableCrud);
					{
						new EngineCrudBuilder().init(crud);
					}
					GSVBox tableSelectedRow = new GSVBox(leftTables).select(GenericWindow::getTableCrudSelectedRow);
					{
						new GenericCrudBuilder().init(tableSelectedRow);
					}

					GSHBox commandPanel = new GSHBox(leftTables).setSpacing(5);
					{
						new GSButton(commandPanel, "Flush", GenericWindow::flush);
						new GSButton(commandPanel, "Cancel", GenericWindow::cancel);
						new GSButton(commandPanel, "Mount", GenericWindow::mount);
						new GSButton(commandPanel, "Unmount", GenericWindow::unmount);
						new GSButton(commandPanel, "ShiftTs", GenericWindow::shiftTs);
					}

				}
			}

		}
	}

	public GenericWindow build(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine cocClient) {
		TableCellTableModel<Integer, Integer> tableModel = new TableCellTableModel<>(FXCollections.observableArrayList(0, 1, 2, 3), FXCollections.observableArrayList(0, 1, 2), item -> col -> {
			TextTableModel<Integer, Integer> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(5, 8, 9, 6), FXCollections.observableArrayList(1, 1, 1, 1),
					item2 -> column -> new ReadOnlyStringWrapper("Cell : " + item2 + " " + column), null, null, null);
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, col -> new ReadOnlyStringWrapper("col :" + col), item -> new ReadOnlyStringWrapper("item :" + item), null);

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(20);
		table.getFirstRowHeight().setValue(30);
		GenericWindow win = new GenericWindow(cocClient, new ReadOnlyObjectWrapper<Table>(table), width, height);
		return win;
	}

	public GenericWindow buildWithGeneric(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(engine)),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(itemTableCell.getComponents()), item2 -> column -> new ReadOnlyStringWrapper(
							"" + item2), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null);
					Table tab = textTableModel.createTable();
					return new ReadOnlyObjectWrapper<Table>(tab);
				}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.createTable();
		table.getFirstRowHeight().setValue(30);
		table.getFirstColumnWidth().setValue(200);
		table.getRowHeight().setValue(45);
		table.getColumnWidth().setValue(310);

		Crud crud = new GenericCrud(new SimpleObjectProperty<Table>(table), engine);
		GenericWindow win = new GenericWindow(crud, width, height);
		return win;
	}
}
