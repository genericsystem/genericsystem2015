package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.gsadmin.TableBuilder.TextCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public abstract class CellBuilder<T> implements Builder {
	Cell<T> build(ObservableValue<T> observableModel, TableStyle tableStyle) {
		return observableModel != null ? new Cell<T>(observableModel, getStyle(tableStyle)) : null;
	}

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	public static class TextCellBuilder extends CellBuilder<String> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSLabel(cellPanels, Cell<String>::getObservableModel);
		}
	}

	public static class FirstRowTextCellBuilder extends TextCellBuilder {

		@Override
		public ObservableValue<String> getStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}
	}

	public static class RowFirstCellTextCellBuilder extends TextCellBuilder {

		@Override
		public ObservableValue<String> getStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}
	}

	public static class FirstRowFirstCellTextCellBuilder extends TextCellBuilder {

		@Override
		public ObservableValue<String> getStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}
	}

	public static class TableCellBuilder<T> extends CellBuilder<Table> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder<>()::init);
		}
	}
}
