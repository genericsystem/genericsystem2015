package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.Cell.TextCell;
import org.genericsystem.gsadmin.Stylable.Listable;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;

public class Row extends Listable<Cell> {

	public static void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(TextCell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(TextCell::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(TextCell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(TextCell::getStyleClass);
	}

	public Row(ObservableValue<Cell> firstCell, ObservableList<Cell> cells, ObservableValue<String> styleClass) {
		super(styleClass, firstCell, cells);
	}

	public static class ExtendedRow extends Row {

		public static void init(Element<?> rowPanel) {
			new GSHBox(rowPanel).select(Row::getFirstElement).include(TextCell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(TextCell::getStyleClass);
			new GSHBox(rowPanel).forEach(Row::getElements).include(ExtendedCell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(ExtendedCell::getStyleClass);
		}

		public ExtendedRow(ObservableValue<Cell> firstCell, ObservableList<Cell> cells, ObservableValue<String> styleClass) {
			super(firstCell, cells, styleClass);
		}
	}
}