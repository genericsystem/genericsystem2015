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
import org.genericsystem.ui.table.Window;

public abstract class GSTable extends GSVBox {

	public GSTable(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		GSSCrollPane scrollPane = new GSSCrollPane(this).setStyleClass("scrollable");
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setPrefHeight(Table::getParentHeight).setPrefWidth(Table::getParentWidth);// .setPrefWidth(getSuperPrefWidth()).setPrefHeight(getSuperPrefHeight());
			{
				new GSTextCellRow(tablePanel).select(Table::getFirstElement).setStyleClass(Row::getStyleClass).setMinHeight(Row::getFirstRowHeight).setMaxHeight(Row::getFirstRowHeight).setPrefHeight(Row::getFirstRowHeight);
				createSelectionHBox(tablePanel).forEach(Table::getElements).setMinHeight(Row::getRowHeight).setMaxHeight(Row::getRowHeight).setPrefHeight(Row::getRowHeight);
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
			return app -> ((Window) app).getWidth();
			// return app -> new SimpleObjectProperty<>(900);
		}

		@Override
		protected <M> Function<M, ObservableValue<Number>> getSuperPrefHeight() {
			// = return app -> ((Window) app).getHeight();
			return app -> ((Window) app).getHeight();
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
			return app -> new SimpleObjectProperty<>(600);
		}

		@Override
		public GSHBox createSelectionHBox(Element<?> parent) {
			return new GSTableCellRow(parent);
		}
	}

}
