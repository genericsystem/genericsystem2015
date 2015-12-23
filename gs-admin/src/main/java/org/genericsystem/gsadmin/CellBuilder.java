package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public interface CellBuilder<T> {
	default Cell<T> build(ObservableValue<T> observableString, ObservableValue<String> styleClass) {
		return observableString != null ? new Cell(observableString, styleClass) : null;
	}

	public static interface TextCellBuilder extends CellBuilder<String> {
		public default void init(Element<HBox> cellPanels) {
			new GSLabel(cellPanels, Cell<String>::getObservableString).setPrefWidth(200);
		}
	}

	public static interface TableCellBuilder extends CellBuilder<Table> {
		public default void init(Element<HBox> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableString).include(new TableBuilder() {
			}::init);
		}
	}

}
