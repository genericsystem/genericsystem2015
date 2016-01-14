package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericRowBuilders.TableCellRowBuilder;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellFirstRowBuilder;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellRowBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.TableBuilder;

public abstract class GenericTableBuilders {
	
	public static class TextCellTableBuilder extends TableBuilder<Generic, Generic, String> {
		@Override
		public void init(Element<?> parent) {
			GSSCrollPane scrollPane = new GSSCrollPane(parent).setStyleClass("scrollable");
			{
				GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setSuperPrefWidth(getSuperPrefWidth()).setSuperPrefHeight(getSuperPrefHeight());;
				{
					new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(GenericRow::getStyleClass).setMinHeight(Table::getFirstRowHeight).setMaxHeight(Table::getFirstRowHeight)
							.setPrefHeight(Table::getFirstRowHeight);
					new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(GenericRow::getStyleClass).setMinHeight(Table::getRowHeight).setMaxHeight(Table::getRowHeight).setPrefHeight(Table::getRowHeight)
							.addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRow);
				}
			}
		}
		
		@Override
		protected RowBuilder<Generic, String> getRowBuilder() {
			return new TextCellRowBuilder<Generic>();
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
			return table -> ((Table) table).getColumnWidth();
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			return table -> ((Table) table).getRowHeight();
		}

	}

	public static class TableCellTableBuilder extends TableBuilder<Generic, Generic, Table> {
		@Override
		public void init(Element<?> parent) {
			GSSCrollPane scrollPane = new GSSCrollPane(parent).setStyleClass("scrollable");
			{
				GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setSuperPrefWidth(getSuperPrefWidth()).setSuperPrefHeight(getSuperPrefHeight());;
				{
					new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(GenericRow::getStyleClass).setMinHeight(Table::getFirstRowHeight).setMaxHeight(Table::getFirstRowHeight)
							.setPrefHeight(Table::getFirstRowHeight);
					new GSHBox(tablePanel).forEach(Table::getElements).include(getRowBuilder()::init).setStyleClass(GenericRow::getStyleClass).setMinHeight(Table::getRowHeight).setMaxHeight(Table::getRowHeight).setPrefHeight(Table::getRowHeight)
							.addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRow);
				}
			}
		}
		
		@Override
		protected RowBuilder<Generic, Table> getRowBuilder() {
			return new TableCellRowBuilder();
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
			return app -> new SimpleObjectProperty<Number>(900);
			// return app -> ((GenericWindow) app).getWidth();
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			return app -> ((GenericWindow) app).getHeight();
		}
	}
}
