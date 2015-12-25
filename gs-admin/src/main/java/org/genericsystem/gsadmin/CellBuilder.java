package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.TableBuilder.TextCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;

public abstract class CellBuilder<T> implements Builder {
	Cell<T> build(CellModel<T> cellModel) {
		return cellModel.getObservableModel() != null ? new Cell<T>(cellModel.getObservableModel(), getStyle(cellModel)) : null;
	}

	public ObservableValue<String> getStyle(CellModel<T> model) {
		return model.getTableStyle().cell;
	}

	public static class TextCellBuilder extends CellBuilder<String> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSLabel(cellPanels, Cell<String>::getObservableModel).setPrefWidth(200);
		}
	}

	public static class FirstRowTextCellBuilder extends TextCellBuilder {

		@Override
		public ObservableValue<String> getStyle(CellModel<String> model) {
			return model.getTableStyle().firstRowCell;
		}
	}

	public static class RowFirstCellTextCellBuilder extends TextCellBuilder {

		@Override
		public ObservableValue<String> getStyle(CellModel<String> model) {
			return model.getTableStyle().firstCell;
		}
	}

	public static class FirstRowFirstCellTextCellBuilder extends TextCellBuilder {

		@Override
		public ObservableValue<String> getStyle(CellModel<String> model) {
			return model.getTableStyle().firstRowFirstCell;
		}
	}

	// @Override
	// ObservableValue<String> getFirstCellStyle(RowModel<COL, U, String> model) {
	// return model.getTableStyle().firstRowFirstCell;
	// }
	//
	// @Override
	// ObservableValue<String> getCellStyle(RowModel<COL, U, String> model) {
	// return model.getTableStyle().firstRowCell;
	// }

	public static class TableCellBuilder<T> extends CellBuilder<Table> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder<>()::init);
		}

	}
}
