package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.RowCreation.ExtendedRowCreation;
import org.genericsystem.gsadmin.RowCreation.FirstRowCreation;
import org.genericsystem.gsadmin.Stylable.Listable;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;

public class Row extends Listable<Cell> {
	static {
		Func.put(Row.class, RowCreation.class, new RowCreation() {
		});
		Func.put(FirstRow.class, RowCreation.class, new FirstRowCreation() {
		});
		Func.put(ExtendedRow.class, RowCreation.class, new ExtendedRowCreation() {
		});
	}

	public static void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(Cell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(Cell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell::getStyleClass);
	}

	public Row(ObservableValue<Cell> firstCell, ObservableList<Cell> cells, ObservableValue<String> styleClass) {
		super(styleClass, firstCell, cells);
	}

	public static class FirstRow extends Row {

		public FirstRow(ObservableValue<Cell> firstCell, ObservableList<Cell> cells, ObservableValue<String> styleClass) {
			super(firstCell, cells, styleClass);
		}

	}

	public static class ExtendedRow extends Row {

		public static void init(Element<?> rowPanel) {
			new GSHBox(rowPanel).select(Row::getFirstElement).include(Cell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell::getStyleClass);
			new GSHBox(rowPanel).forEach(Row::getElements).include(ExtendedCell::initCell).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell::getStyleClass);
		}

		public ExtendedRow(ObservableValue<Cell> firstCell, ObservableList<Cell> cells, ObservableValue<String> styleClass) {
			super(firstCell, cells, styleClass);
		}
	}
}