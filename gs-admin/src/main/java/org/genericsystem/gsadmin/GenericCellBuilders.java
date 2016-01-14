package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Button;

import org.genericsystem.gsadmin.GenericTableBuilders.TextCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.CellBuilder;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;

public class GenericCellBuilders {
	public static class TableCellBuilder<T> extends CellBuilder<Table> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder()::init);
		}
	}
	
	public static class ButtonCellBuilder extends CellBuilder<String> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSButton(cellPanels, Cell<String>::getObservableModel).setAction(GenericRow::delete).addBoot(Button::paddingProperty, new Insets(2, 2, 2, 2));
		}
	}

	public static class RowLastCellButtonCellBuilder extends ButtonCellBuilder {
		@Override
		public ObservableValue<String> getStyle(TableStyle tableStyle) {
			return tableStyle.lastCell;
		}
	}
}
