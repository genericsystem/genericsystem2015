package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.table.GenericModel;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
import org.genericsystem.kernel.Engine;

public class AppModel extends GenericModel {

	private final ObservableValue<TypeTableModel> typeTableModel;

	public AppModel(Engine engine, Generic type, ObservableList<Generic> attributes) {
		super(engine);
		typeTableModel = new ReadOnlyObjectWrapper<TypeTableModel>(new TypeTableModel(type, attributes));
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	/*********************************************************************************************************************************/

	public ObservableValue<TypeTableModel> getTypeTableModel() {
		return typeTableModel;
	}
}
