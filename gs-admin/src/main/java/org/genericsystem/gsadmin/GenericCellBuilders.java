package org.genericsystem.gsadmin;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.CellBuilder;
import org.genericsystem.ui.table.Table;
import org.genericsystem.gsadmin.GenericTableBuilders.*;

public class GenericCellBuilders {
	public static class TableCellBuilder<T> extends CellBuilder<Table> {
		@Override
		public void init(Element<?> cellPanels) {
			new GSVBox(cellPanels).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder()::init);
		}
	}
}
