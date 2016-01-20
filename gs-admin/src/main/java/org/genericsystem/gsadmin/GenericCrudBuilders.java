package org.genericsystem.gsadmin;

import org.genericsystem.gsadmin.GenericTableBuilders.TableCellTableBuilder;
import org.genericsystem.gsadmin.GenericTableBuilders.TableCellTableBuilder2;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.table.CrudBuilder;

public class GenericCrudBuilders {

	public static class EngineCrudBuilder extends CrudBuilder {
		@Override
		protected void createTableBuilder(Element parent) {
			new TableCellTableBuilder().init(parent);
		}
	}

	public static class GenericCrudBuilder extends CrudBuilder {
		@Override
		protected void createTableBuilder(Element parent) {
			new TableCellTableBuilder2().init(parent);
		}
	}
}
