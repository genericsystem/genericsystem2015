package org.genericsystem.gsadmin;

import java.awt.TextField;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
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
//					GSHBox formPanel = new GSHBox(containerTables).setSpacing(10);
//					{
//						new GSTextField(formPanel).setPrefWidth(300);
//						new GSButton(formPanel, "Add").setPrefWidth(100);
//					}
//					
					GSVBox table = new GSVBox(containerTables).select(Window::getTable);
					{
						new TableCellTableBuilder<>().init(table);
					}

					GSVBox table2 = new GSVBox(containerTables).select(Window::getTableSelectedRow);
					{
						new TableCellTableBuilder<>().init(table2);
					}
					
					GSHBox createPanel = new GSHBox(containerTables);
					{
						new GSButton(createPanel, "create");
					}
				}
			}
		}
	}

	public Window build(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine c) {
		TableCellTableModel<Integer, Integer> tableModel = new TableCellTableModel<>(FXCollections.observableArrayList(0, 1, 2, 3), FXCollections.observableArrayList(0, 1, 2), item -> col -> {
			TextTableModel<Integer, Integer> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(5, 8, 9, 6), FXCollections.observableArrayList(1, 1, 1, 1),
					item2 -> column -> new ReadOnlyStringWrapper("Cell : " + item2 + " " + column), null, null);
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, col -> new ReadOnlyStringWrapper("col :" + col), item -> new ReadOnlyStringWrapper("item :" + item));
		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(300);
		table.getRowHeight().setValue(100);
		table.getFirstRowHeight().setValue(50);
		Window win = new Window(new ReadOnlyObjectWrapper<Table>(table), width, height);
		return win;
	}

	public Window buildWithGeneric(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {

		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(engine.getObservableInstances(), engine.getObservableAttributes(), itemTableCell -> columnTableCell -> {
			TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(), null, null, column -> new ReadOnlyStringWrapper("" + column));
			Table tab = textTableModel.createTable();
			return new ReadOnlyObjectWrapper<Table>(tab);
		}, column -> new ReadOnlyStringWrapper("" + column), firstColumString -> new ReadOnlyStringWrapper("" + firstColumString));

		Table table = tableModel.createTable();
		table.getColumnWidth().setValue(400);
		table.getRowHeight().setValue(50);
		table.getFirstRowHeight().setValue(50);
		Window win = new Window(new ReadOnlyObjectWrapper<Table>(table), width, height);
		return win;
	}
}
