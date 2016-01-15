package org.genericsystem.gsadmin;

import org.genericsystem.gsadmin.GenericTableBuilders.TableCellTableBuilder;
import org.genericsystem.gsadmin.GenericTableBuilders.TableCellTableBuilder2;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Builder;

public class GenericCrudBuilders  {

	public static class EngineCrudBuilder implements Builder{

		@Override
		public void init(Element<?> parent) {
			GSHBox Hb = new GSHBox(parent);
			{
				GSVBox vb = new GSVBox(Hb);
				{
					GSHBox formPanelEngine = new GSHBox(vb).setSpacing(10);
					{
						new GSTextField(formPanelEngine).bindTextProperty(Crud::getName).setPrefWidth(300);
						new GSButton(formPanelEngine, "Add", Crud::add);
					}

					GSVBox table = new GSVBox(vb).select(Crud::getTable);
					{
						new TableCellTableBuilder().init(table);
					}
				}

				GSVBox editTable = new GSVBox(Hb).select(GenericWindow::getEditTableCrudSelectedRow);
				{
					GSVBox table = new GSVBox(editTable).select(Crud::getTable);
					{
						new TableCellTableBuilder().init(table);
					}
				}
			}
		}
		
	}
	
	public static class GenericCrudBuilder implements Builder{

		@Override
		public void init(Element<?> parent) {
			GSHBox Hb = new GSHBox(parent);
			{
				GSVBox vb = new GSVBox(Hb);
				{
					GSHBox formPanelEngine = new GSHBox(vb).setSpacing(10);
					{
						new GSTextField(formPanelEngine).bindTextProperty(Crud::getName).setPrefWidth(300);
						new GSButton(formPanelEngine, "Add", Crud::add);
					}

					GSVBox table = new GSVBox(vb).select(Crud::getTable);
					{
						new TableCellTableBuilder2().init(table);
					}
				}

				GSVBox editTable = new GSVBox(Hb).select(GenericWindow::getEditTableCrudSelectedRow);
				{
					GSVBox table = new GSVBox(editTable).select(Crud::getTable);
					{
						new TableCellTableBuilder().init(table);
					}
				}
			}
		}
		
	}
}
