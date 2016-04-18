package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends GenericModel {

	private final ObservableList<InstanceRowModel> instanceModels;

	public TypeTableModel(Generic type, ObservableList<Generic> attributes) {
		super(type);
		instanceModels = new Transformation2<>(type.getObservableSubInstances(), g -> new InstanceRowModel(g, attributes) /* , carModel -> new Observable[] { CarModel.() } */);
	}

	/********************/

	public ObservableList<InstanceRowModel> getInstanceModels() {
		return instanceModels;
	}

}
