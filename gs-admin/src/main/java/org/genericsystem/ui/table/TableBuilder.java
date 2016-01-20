package org.genericsystem.ui.table;

import java.util.function.Function;
import javafx.beans.value.ObservableValue;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellFirstRowBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;

public abstract class TableBuilder<ITEM, COL, T> implements Builder {

	@Override
	public void init(Element<?> parent) {
		GSSCrollPane scrollPane = new GSSCrollPane(parent).setStyleClass("scrollable");
		{
			GSVBox tablePanel = new GSVBox(scrollPane).setStyleClass(Table::getStyleClass).setSuperPrefWidth(getSuperPrefWidth()).setSuperPrefHeight(getSuperPrefHeight());
			{
				new GSHBox(tablePanel).select(Table::getFirstElement).include(new TextCellFirstRowBuilder<>()::init).setStyleClass(Row::getStyleClass).setMinHeight(Table::getFirstRowHeight).setMaxHeight(Table::getFirstRowHeight)
				.setPrefHeight(Table::getFirstRowHeight);
				createSelectionHBox(tablePanel).forEach(Table::getElements).setStyleClass(Row::getStyleClass).setMinHeight(Table::getRowHeight).setMaxHeight(Table::getRowHeight).setPrefHeight(Table::getRowHeight);
			}
		}
	}

	protected abstract GSHBox createSelectionHBox(Element<?> parent);

	protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefWidth();

	protected abstract <M> Function<M, ObservableValue<Number>> getSuperPrefHeight();
}