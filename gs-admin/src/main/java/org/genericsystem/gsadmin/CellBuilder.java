package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.TableBuilder.TextCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;

public abstract class CellBuilder<T> implements Builder {
	Cell<T> build(ObservableValue<T> observableString, ObservableValue<String> styleClass) {
		return observableString != null ? new Cell<>(observableString, styleClass) : null;
	}

	public static class TextCellBuilder extends CellBuilder<String> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSLabel(cellPanels, Cell<String>::getObservableString).setPrefWidth(200);
		}
	}

	public static class TableCellBuilder extends CellBuilder<Table> {
		@Override
		public void init(Element<?> cellPanels) {
			GSSCrollPane scrollPane = new GSSCrollPane(cellPanels);
			{
				new GSVBox(scrollPane).select(Cell<Table>::getObservableString).include(new TextCellTableBuilder<>()::init);
			}
		}
	}

}
