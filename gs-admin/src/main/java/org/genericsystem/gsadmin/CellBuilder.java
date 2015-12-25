package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.TableBuilder.TextCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public abstract class CellBuilder<T> implements Builder {
	Cell<T> build(ObservableValue<T> observableModel, ObservableValue<String> styleClass) {
		return observableModel != null ? new Cell<T>(observableModel, styleClass) : null;
	}

	public static class TextCellBuilder<T> extends CellBuilder<String> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSLabel(cellPanels, Cell<String>::getObservableModel).setPrefWidth(200);
		}
	}

	public static class TableCellBuilder<T> extends CellBuilder<Table> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder<>()::init);
		}
	}
}
