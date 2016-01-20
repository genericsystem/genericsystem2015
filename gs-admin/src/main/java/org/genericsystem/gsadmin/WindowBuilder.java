package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.gsadmin.GenericCrudBuilders.EngineCrudBuilder;
import org.genericsystem.gsadmin.GenericCrudBuilders.GenericCrudBuilder;
import org.genericsystem.gsadmin.TableBuilderModel.TableCellTableModel;
import org.genericsystem.gsadmin.TableBuilderModel.TextTableModel;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
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
						new GSLabel(commandPanel, GenericWindow::getCacheLevel);// .setObservableTextProperty(GenericWindow::getCacheLevel);
					}

				}
			}

		}
	}

	public GenericWindow buildWithGeneric(ObservableValue<? extends Number> width, ObservableValue<? extends Number> height, CocClientEngine engine) {
		TableCellTableModel<Generic, Generic> tableModel = new TableCellTableModel<>(engine.getObservableSubInstances(), engine.getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(engine)),
				itemTableCell -> columnTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(itemTableCell.getObservableHolders(columnTableCell), FXCollections.observableArrayList(itemTableCell.getComponents()), null, null,
							firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null, null);
					Table tab = textTableModel.buildTable();
					return new ReadOnlyObjectWrapper<Table>(tab);
				}, firstRowString -> new ReadOnlyStringWrapper("" + firstRowString), null,

				itemTableCell -> {
					TextTableModel<Generic, Generic> textTableModel = new TextTableModel<>(FXCollections.observableArrayList(itemTableCell), FXCollections.observableArrayList(itemTableCell.getComponents()), item -> col -> new ReadOnlyStringWrapper(""
							+ col), null, firstColumString -> new ReadOnlyStringWrapper("" + firstColumString), null, null);
					Table tab = textTableModel.buildTable();
					tab.getFirstColumnWidth().setValue(195);

					tab.getFirstRowHeight().setValue(30);
					tab.getRowHeight().setValue(75);

					return new ReadOnlyObjectWrapper<Table>(tab);
				}, column -> new ReadOnlyStringWrapper("Delete"));

		Table table = tableModel.buildTable();
		table.getFirstRowHeight().setValue(30);
		table.getFirstColumnWidth().setValue(300);
		table.getRowHeight().setValue(80);
		table.getColumnWidth().setValue(310);

		GenericCrud crud = new GenericCrud(new SimpleObjectProperty<Table>(table), engine);
		GenericWindow win = new GenericWindow(crud, width, height);
		return win;
	}
}
