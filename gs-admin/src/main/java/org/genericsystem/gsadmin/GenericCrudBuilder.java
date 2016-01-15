package org.genericsystem.gsadmin;

import org.genericsystem.gsadmin.GenericTableBuilders.TableCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Builder;

public class GenericCrudBuilder implements Builder {

	@Override
	public void init(Element<?> parent) {
		
			GSHBox formPanelEngine = new GSHBox(parent).setSpacing(10);
			{
				new GSTextField(formPanelEngine).bindTextProperty(Crud::getName).setPrefWidth(300);
				new GSButton(formPanelEngine, "Add", Crud::add);
			}

			GSVBox table = new GSVBox(parent).select(Crud::getTable);
			{
				new TableCellTableBuilder().init(table);
			}
		
//		GSVBox editTable = new GSVBox(parent).select(GenericWindow::getEditTableCrudSelectedRow);
//		{
//			new GenericCrudBuilder().init(editTable);
//		}
		
	}
}
