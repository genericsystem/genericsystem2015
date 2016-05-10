package org.genericsystem.gsadmin;

import org.genericsystem.gsadmin.GSTable.GSEditTable;
import org.genericsystem.gsadmin.GSTable.GSTableCellTableEngine;
import org.genericsystem.gsadmin.GSTable.GSTableCellTableGeneric;
import org.genericsystem.reactor.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSComboBox;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class GSCrud extends GSVBox {

	public GSCrud(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {

		GSVBox vb = new GSVBox(this).setSpacing(10);
		{
			GSHBox formPanelEngine = new GSHBox(vb).setSpacing(5);
			{
				new GSTextField(formPanelEngine).bindTextProperty(GenericCrud::getName).setPrefWidth(300).setStyleClass("blackBorder");
				new GSButton(formPanelEngine, "Add", GenericCrud::add).setStyleClass("blackBorder");
				new GSComboBox<>(formPanelEngine, GenericCombobox::getItems).setStyleClass("blackBorder").setStyleClass("ComboBox").forEach(GenericCrud::getListCombobox)
						.addReversedBinding(c -> c.getSelectionModel().selectedItemProperty(), GenericCombobox::getSelectedItem);
			}
			GSHBox tablesPanel = new GSHBox(vb).setSpacing(10);
			{
				initGSGenericTable(tablesPanel);
				new GSEditTable(tablesPanel).select(GenericCrud::getEditTable).setStyleClass("blackBorder").setStyleClass("tablePadding");
			}
		}

	}

	protected void initGSGenericTable(GSHBox vb) {
		new GSTableCellTableGeneric(vb).select(GenericCrud::getTable).setStyleClass("blackBorder").setStyleClass("tablePadding");
	}

	public static class GSEngineCrud extends GSCrud {

		public GSEngineCrud(Element<?> parent) {
			super(parent);
		}

		@Override
		protected void initGSGenericTable(GSHBox vb) {
			new GSTableCellTableEngine(vb).select(GenericCrud::getTable).setStyleClass("blackBorder").setStyleClass("tablePadding");
		}
	}
}
