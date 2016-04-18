package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

public class InstanceRowModel extends GenericModel {

	private ObservableList<InstanceAttributeCellModel> instanceAttributesModels;

	public InstanceRowModel(Generic instance, ObservableList<Generic> attributes) {
		super(instance);
		instanceAttributesModels = new Transformation2<>(attributes, attribute -> new InstanceAttributeCellModel(instance, attribute));
	}

	public ObservableList<InstanceAttributeCellModel> getInstanceAttributeModels() {
		return instanceAttributesModels;
	}

	public void remove() {
		getGeneric().remove();
	}
}
