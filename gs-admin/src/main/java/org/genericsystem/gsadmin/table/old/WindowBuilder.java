package org.genericsystem.gsadmin.table.old;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.table.old.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.table.old.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.table.old.TableBuilderModel.TextTableModel;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class WindowBuilder implements Builder {
	@Override
	public void init(Element<?> parent) {
		GSVBox mainPanel = new GSVBox(parent).setPrefHeight(Window::getHeight);
		{
			GSSCrollPane scrollPane = new GSSCrollPane(mainPanel).setStyleClass("scrollable");
			{
				GSVBox containerTables = new GSVBox(scrollPane).setMinHeight(500);
				{
					GSVBox table = new GSVBox(containerTables).select(Window::getTable);
					{
						GSHBox formPanel = new GSHBox(table).setSpacing(10).select(Table::getSelectedRow);// .select(Window::getSelectedRow);//.select(Window::getSelectedRow);
						{
							new GSTextField(formPanel).bindTextProperty(Row::getName).setPrefWidth(300);
							new GSButton(formPanel, "Add", Row::add);
						}
						new TableCellTableBuilder<>().init(table);					
					}
					
					GSVBox tableSelectedRow = new GSVBox(containerTables).select(Window::getTableSelectedRow);
					{
						new TableCellTableBuilder<>().init(tableSelectedRow);
					}	
					
					GSHBox commandPanel = new GSHBox(containerTables).setSpacing(10);
					{
						 new GSButton(commandPanel, "Flush",Window::flush);
						 new GSButton(commandPanel, "Cancel",Window::cancel);
						 new GSButton(commandPanel, "Mount",Window::mount);
						 new GSButton(commandPanel, "Unmount",Window::unmount);
					}
				}
			}
		}
	}

	public Window build(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine cocClient) {
		TableCellTableModel<Integer, Integer> tableModel = new TableCellTableModel<>(FXCollections.observableArrayList(0, 1, 2, 3), FXCollections.observableArrayList(0, 1, 2), item -> col -> {
			TextTableModel<Integer, Integer> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(5, 8, 9, 6), FXCollections.observableArrayList(1, 1, 1, 1),
					item2 -> column -> new ReadOnlyStringWrapper("Cell : " + item2 + " " + column), null, null);
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, col -> new ReadOnlyStringWrapper("col :" + col), item -> new ReadOnlyStringWrapper("item :" + item));
		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(100);
		table.getFirstRowHeight().setValue(30);
		Window win = new Window(cocClient,new ReadOnlyObjectWrapper<Table>(table), width, height);
		return win;
	}

	public Window buildWithGeneric(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {

		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(engine.getObservableInstances(), engine.getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(engine)), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, column -> new ReadOnlyStringWrapper("" + column));
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, column -> new ReadOnlyStringWrapper("" + column), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString));

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(120);
		table.getRowHeight().setValue(20);
		table.getFirstRowHeight().setValue(30);
		Window win = new Window(engine,new ReadOnlyObjectWrapper<Table>(table), width, height);
		return win;
		
	}
}
