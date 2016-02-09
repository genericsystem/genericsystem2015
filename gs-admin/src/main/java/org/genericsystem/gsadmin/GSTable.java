package org.genericsystem.gsadmin;

import javafx.scene.layout.HBox;

import org.genericsystem.gsadmin.GSRow.GSTableCellRow;
import org.genericsystem.gsadmin.GSRow.GSTextCellFirstRow;
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

		createFirstRow(this);

		GSSCrollPane scrollPane = new GSSCrollPane(this).setStyleClass("scrollable");
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass)
					.setPrefHeight(Table::getParentHeight).setPrefWidth(Table::getParentWidth);// .setPrefWidth(getSuperPrefWidth()).setPrefHeight(getSuperPrefHeight());
			{
				createRows(tablePanel);
			}
		}
	}

	protected GSHBox createRows(Element<?> parent){
		return (GSHBox) new GSTableCellRow(parent).forEach(Table::getElements).setStyleClass(Row::getStyleClass).setMinHeight(Row::getRowHeight).setMaxHeight(Row::getRowHeight).setPrefHeight(Row::getRowHeight);
	}

	protected void createFirstRow(Element<?> parent) {
		new GSTextCellFirstRow(parent).select(Table::getFirstElement).setStyleClass(Row::getStyleClass)
				.setMinHeight(Row::getFirstRowHeight).setMaxHeight(Row::getFirstRowHeight)
				.setPrefHeight(Row::getFirstRowHeight);
	}

	protected void createLastRow(Element<?> parent){}
	
	public static class GSTextCellTable extends GSTable {

		public GSTextCellTable(Element<?> parent) {
			super(parent);
		}

		@Override
		public GSHBox createRows(Element<?> parent) {
			return (GSHBox) new GSTextCellRow(parent).forEach(Table::getElements).setStyleClass(Row::getStyleClass).setMinHeight(Row::getRowHeight).setMaxHeight(Row::getRowHeight).setPrefHeight(Row::getRowHeight).addActionBinding(HBox::onMouseClickedProperty, GenericRow::selectRowGenericTable);
		}
	}

	public static class GSTableCellTableEngine extends GSTable {

		public GSTableCellTableEngine(Element<?> parent) {
			super(parent);
		}

		@Override
		public GSHBox createRows(Element<?> parent) {
			return (GSHBox) super.createRows(parent).addActionBinding(HBox::onMouseClickedProperty,
					GenericRow::selectRowEngineTable);
		}
	}

	public static class GSTableCellTableGeneric extends GSTableCellTableEngine {

		public GSTableCellTableGeneric(Element<?> parent) {
			super(parent);
		}

		@Override
		public GSHBox createRows(Element<?> parent) {
			return (GSHBox) super.createRows(parent).addActionBinding(HBox::onMouseClickedProperty,
					GenericRow::selectRowGenericTable);
		}
	}

	public static class GSEditTableCellTable extends GSTableCellTableEngine {

		public GSEditTableCellTable(Element<?> parent) {
			super(parent);
		}

		@Override
		public GSHBox createRows(Element<?> parent) {
			return super.createRows(parent);// new GSEditableCellRow(parent);
		}
	}
}
