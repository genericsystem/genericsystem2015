package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.gsadmin.CellCreation.ExtendedCellCreation;
import org.genericsystem.gsadmin.CellCreation.FirstCellCreation;
import org.genericsystem.gsadmin.CellCreation.FirstRowFirstCellCreation;
import org.genericsystem.gsadmin.CellCreation.RowFirstCellCreation;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public class Cell extends Stylable {
	static {
		Func.put(Cell.class, CellCreation.class, new CellCreation() {
		});
		Func.put(FirstRowFirstCell.class, CellCreation.class, new FirstRowFirstCellCreation() {
		});
		Func.put(FirstCell.class, CellCreation.class, new FirstCellCreation() {
		});
		Func.put(RowFirstCell.class, CellCreation.class, new RowFirstCellCreation() {
		});
		Func.put(ExtendedCell.class, CellCreation.class, new ExtendedCellCreation() {
		});
	}
	private final ObservableValue<String> observableString;

	public static void init(Element<HBox> cellPanels) {
		new GSLabel(cellPanels, Cell::getObservableString).setPrefWidth(200);
	}

	public Cell(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
		super(styleClass);
		this.observableString = observableString;
	}

	public ObservableValue<String> getObservableString() {
		return observableString;
	}

	public static class FirstRowFirstCell extends Cell {

		public FirstRowFirstCell(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			super(observableString, styleClass);
		}
	}

	public static class FirstCell extends Cell {

		public FirstCell(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			super(observableString, styleClass);
		}
	}

	public static class RowFirstCell extends Cell {

		public RowFirstCell(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			super(observableString, styleClass);
		}
	}

	public static class ExtendedCell extends Cell {
		private final ObservableValue<Table> tableList;

		public static void initCell(Element<HBox> cellPanels) {
			new GSVBox(cellPanels).select(ExtendedCell::getTableList).include(Table::init);
		}

		public <ITEM, COL> ExtendedCell(ObservableValue<String> observableString, ObservableValue<String> styleClass, TableModel<ITEM, COL> tableModel) {
			super(observableString, styleClass);
			assert tableModel != null;
			tableList = new ReadOnlyObjectWrapper<>(Func.get(Table.class, TableCreation.class).create(tableModel));
		}

		public ObservableValue<Table> getTableList() {
			return tableList;
		}
	}
}