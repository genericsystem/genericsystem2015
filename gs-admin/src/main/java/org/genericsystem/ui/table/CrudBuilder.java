package org.genericsystem.ui.table;

import org.genericsystem.gsadmin.GenericCrud;
import org.genericsystem.gsadmin.GenericTableBuilders.EditTableCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public abstract class CrudBuilder implements Builder {

	@Override
	public void init(Element<?> parent) {
		GSHBox Hb = new GSHBox(parent);
		{
			GSVBox vb = new GSVBox(Hb);
			{
				GSHBox formPanelEngine = new GSHBox(vb).setSpacing(10);
				{
					new GSTextField(formPanelEngine).bindTextProperty(GenericCrud::getName).setPrefWidth(300);
					new GSButton(formPanelEngine, "Add", GenericCrud::add);
				}

				GSVBox table = new GSVBox(vb).select(GenericCrud::getTable);
				{
					createTableBuilder(table);
				}
			}

			GSVBox editTable = new GSVBox(Hb).select(GenericCrud::getEditTable);
			{
				new EditTableCellTableBuilder().init(editTable);
			}
		}
	}

	protected abstract void createTableBuilder(Element parent);
}
