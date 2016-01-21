package org.genericsystem.gsadmin;

import org.genericsystem.gsadmin.GSTable.GSTableCellTable;
import org.genericsystem.gsadmin.GSTable.GSTableCellTable2;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.table.CrudBuilder;

public class GenericCrudBuilders {

	public static class EngineCrudBuilder extends CrudBuilder {
		@Override
		protected void createTableBuilder(Element parent) {
			new GSTableCellTable(parent);
			// new TableCellTableBuilder().init(parent);
		}
	}

	public static class GenericCrudBuilder extends CrudBuilder {
		@Override
		protected void createTableBuilder(Element parent) {
			new GSTableCellTable2(parent);
			// new TableCellTableBuilder2().init(parent);
		}
	}
}
