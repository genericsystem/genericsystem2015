package org.genericsystem.gsadmin;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSVBox;

public class GSCrud extends GSVBox {

	public GSCrud(Element<?> parent) {
		super(parent);
	}

	protected void initGSGenericTable(GSVBox vb) {
		// new TableCellTableBuilder2().init(vb);
		// new GSTableCellTable2(vb).select(GenericCrud::getTable);
	}

	@Override
	protected void initChildren() {
		// GSHBox Hb = new GSHBox(this);
		// {
		// GSVBox vb = new GSVBox(Hb);
		// {
		// GSHBox formPanelEngine = new GSHBox(vb).setSpacing(10);
		// {
		// new GSTextField(formPanelEngine).bindTextProperty(GenericCrud::getName).setPrefWidth(300);
		// new GSButton(formPanelEngine, "Add", GenericCrud::add);
		// }
		//
		// initGSGenericTable(vb);
		// }
		//
		// GSVBox editTable = new GSVBox(Hb).select(GenericCrud::getEditTable);
		// {
		// new EditTableCellTableBuilder().init(editTable);
		// }
		// }
	}

	public static class GSEngineCrud extends GSCrud {

		public GSEngineCrud(Element<?> parent) {
			super(parent);
		}

		@Override
		protected void initGSGenericTable(GSVBox vb) {
			// new TableCellTableBuilder().init(vb);
			// new GSTableCellTable(vb).select(GenericCrud::getTable);
		}

	}

}
