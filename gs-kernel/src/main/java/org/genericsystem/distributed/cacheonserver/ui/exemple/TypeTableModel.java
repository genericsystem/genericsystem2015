package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericModel {

	private final ObservableValue<TitleRowModel> titleRowModel;
	private final ObservableValue<InsertRowModel> insertRowModel;
	private final ObservableList<InstanceRowModel> instanceModels;

	public TypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type);
		titleRowModel = new ReadOnlyObjectWrapper<>(new TitleRowModel(type));
		insertRowModel = new ReadOnlyObjectWrapper<>(new InsertRowModel(type, attributes));
		instanceModels = new Transformation2<>(type.getObservableSubInstances(), g -> new InstanceRowModel(g, attributes) /* , carModel -> new Observable[] { CarModel.() } */);
	}

	/**********************************************************************/

	public ObservableValue<TitleRowModel> getTitleRowModel() {
		return titleRowModel;
	}

	public ObservableValue<InsertRowModel> getInsertRowModel() {
		return insertRowModel;
	}

	public ObservableList<InstanceRowModel> getInstanceModels() {
		return instanceModels;
	}
}
