package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericRowBuilders.TableCellRowBuilder;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellRowBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.TableBuilder;

public abstract class GenericTableBuilders {

	public static class TextCellTableBuilder extends TableBuilder<Generic, Generic, String> {

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

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSHBox(parent).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowGenericTable);
		}

	}

	public static class TableCellTableBuilder extends TableBuilder<Generic, Generic, Table> {

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

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSHBox(parent).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowEngineTable);

		}

	}

	public static class TableCellTableBuilder2 extends TableBuilder<Generic, Generic, Table> {

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

		// @Override
		// public GSHBox setActionSelectionRow(Element<?> parent) {
		// return (GSHBox) new GSHBox(parent).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowGenericTable);
		// }

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSHBox(parent).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowGenericTable);

		}
	}
}
