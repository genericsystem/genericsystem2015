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
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.TableBuilder;
import org.genericsystem.ui.table.Window;

public abstract class GenericTableBuilders {

	public static class TextCellTableBuilder extends TableBuilder<Generic, Generic, String> {

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
			return (GSHBox) new GSHBox(parent).include(new TextCellRowBuilder<>()::init).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowGenericTable);
		}
	}

	public static class TableCellTableBuilder extends TableBuilder<Generic, Generic, Table> {
		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSHBox(parent).include(new TableCellRowBuilder<>()::init).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowEngineTable);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
			return app -> new SimpleObjectProperty<>(900);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			return app -> ((Window) app).getHeight();
		}
	}

	public static class EditTableCellTableBuilder extends TableCellTableBuilder {
		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSHBox(parent).include(new TableCellRowBuilder<>()::init).addBindMetaAction(HBox::onMouseClickedProperty, GenericCrud::test);
		}
	}

	public static class TableCellTableBuilder2 extends TableCellTableBuilder {
		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSHBox(parent).include(new TableCellRowBuilder<>()::init).addBindMetaAction(HBox::onMouseClickedProperty, GenericWindow::selectRowGenericTable);
		}
	}
}
