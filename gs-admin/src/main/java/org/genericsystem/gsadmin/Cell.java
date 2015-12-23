package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.gsadmin.CellCreation.ExtendedCellCreation;
import org.genericsystem.gsadmin.CellCreation.TextCellCreation;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public abstract class Cell<T> extends Stylable {

	static {
		Func.put(TextCell.class, CellCreation.class, new TextCellCreation() {
		});
		Func.put(ExtendedCell.class, CellCreation.class, new ExtendedCellCreation() {
		});
	}

	private final ObservableValue<T> observableModel;

	public Cell(ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		super(styleClass);
		this.observableModel = observableModel;
	}

	public ObservableValue<T> getObservableString() {
		return observableModel;
	}

	public static class TextCell extends Cell<String> {

		public static void init(Element<HBox> cellPanels) {
			new GSLabel(cellPanels, TextCell::getObservableString).setPrefWidth(200);
		}

		public TextCell(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			super(observableString, styleClass);
		}
	}

	public static class ExtendedCell extends Cell<Table> {

		public static void init(Element<HBox> cellPanels) {
			new GSVBox(cellPanels).select(ExtendedCell::getObservableString).include(Table::init);
		}

		public ExtendedCell(ObservableValue<Table> observableTable, ObservableValue<String> styleClass) {
			super(observableTable, styleClass);
		}
	}
}
