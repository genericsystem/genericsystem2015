package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.gsadmin.GSRow.GSTableCellRow;
import org.genericsystem.gsadmin.GSRow.GSTextCellRow;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Table;

public abstract class GSTable extends GSVBox {

	public GSTable(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		GSSCrollPane scrollPane = new GSSCrollPane(this).setStyleClass("scrollable");
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setSuperPrefWidth(getSuperPrefWidth()).setSuperPrefHeight(getSuperPrefHeight());
			{
				new GSTextCellRow(tablePanel).select(Table::getFirstElement).setStyleClass(Row::getStyleClass).setMinHeight(Table::getFirstRowHeight).setMaxHeight(Table::getFirstRowHeight).setPrefHeight(Table::getFirstRowHeight);
				createSelectionHBox(tablePanel).forEach(Table::getElements).setStyleClass(Row::getStyleClass).setMinHeight(Table::getRowHeight).setMaxHeight(Table::getRowHeight).setPrefHeight(Table::getRowHeight);
			}
		}
	}

	protected abstract GSHBox createSelectionHBox(Element<?> parent);

	protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefWidth();

	protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefHeight();

	public static class GSTextCellTable extends GSTable {

		public GSTextCellTable(Element<?> parent) {
			super(parent);
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
			return (GSHBox) new GSTextCellRow(parent).addActionBinding(HBox::onMouseClickedProperty, GenericRow::selectRowGenericTable);
		}

	}

	public static class GSTableCellTable extends GSTable {

		public GSTableCellTable(Element<?> parent) {
			super(parent);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {
			return app -> new SimpleObjectProperty<>(900);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			// = return app -> ((Window) app).getHeight();
			return app -> new SimpleObjectProperty<>(900);
		}

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSTableCellRow(parent).addActionBinding(HBox::onMouseClickedProperty, GenericRow::selectRowEngineTable);
		}
	}

	public static class GSTableCellTable2 extends GSTableCellTable {

		public GSTableCellTable2(Element<?> parent) {
			super(parent);
		}

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSTableCellRow(parent).addActionBinding(HBox::onMouseClickedProperty, GenericRow::selectRowGenericTable);
		}
	}

	public static class GSEditTableCellTable extends GSTableCellTable {

		public GSEditTableCellTable(Element<?> parent) {
			super(parent);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefWidth() {

			return app -> new SimpleObjectProperty<>(600);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			// return app -> ((Window) app).getHeight();
			return app -> new SimpleObjectProperty<>(600);
		}

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return (GSHBox) new GSTableCellRow(parent).addMetaActionBinding(HBox::onMouseClickedProperty, GenericCrud::test);
		}
	}

}
