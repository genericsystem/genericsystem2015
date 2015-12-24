package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public class CellBuilder<T> {
	Cell<T> build(ObservableValue<T> observableString, ObservableValue<String> styleClass) {
		return observableString != null ? new Cell<>(observableString, styleClass) : null;
	}

	public static final class TextCellBuilder extends CellBuilder<String> {
		public void init(Element<HBox> cellPanels) {
			new GSLabel(cellPanels, Cell<String>::getObservableString).setPrefWidth(200);
		}
	}

	public static final class TableCellBuilder extends CellBuilder<Table> {
		public void init(Element<HBox> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableString).include(new TableBuilder<>()::init);
		}
	}

}
