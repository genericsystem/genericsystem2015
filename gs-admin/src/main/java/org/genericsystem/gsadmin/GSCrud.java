package org.genericsystem.gsadmin;

import org.genericsystem.gsadmin.GSTable.GSEditTableCellTable;
import org.genericsystem.gsadmin.GSTable.GSTableCellTable;
import org.genericsystem.gsadmin.GSTable.GSTableCellTable2;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class GSCrud extends GSVBox {

	public GSCrud(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		GSHBox Hb = new GSHBox(this).setSpacing(10);// .setPrefHeight(500);
		{
			GSVBox vb = new GSVBox(Hb);
			{
				GSHBox formPanelEngine = new GSHBox(vb).setSpacing(3);
				{
					new GSTextField(formPanelEngine).bindTextProperty(GenericCrud::getName).setPrefWidth(300);
					new GSButton(formPanelEngine, "Add", GenericCrud::add);
				}

				initGSGenericTable(vb);
			}

			new GSEditTableCellTable(Hb).select(GenericCrud::getEditTable).setSpacing(100);
		}
	}

	protected void initGSGenericTable(GSVBox vb) {
		new GSTableCellTable2(vb).select(GenericCrud::getTable);
	}

	public static class GSEngineCrud extends GSCrud {

		public GSEngineCrud(Element<?> parent) {
			super(parent);
		}

		@Override
		protected void initGSGenericTable(GSVBox vb) {
			new GSTableCellTable(vb).select(GenericCrud::getTable);
		}
	}
}
