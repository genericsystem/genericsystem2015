package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

public class InstanceRowModel extends GenericModel {

	private ObservableList<InstanceAttributeCellModel> instanceAttributesModels;

	public InstanceRowModel(Generic instance, ObservableList<Generic> attributes, Function<Generic, InstanceAttributeCellModel> cellBuilder) {
		super(instance);
		instanceAttributesModels = new Transformation2<>(attributes, cellBuilder);
	}

	public ObservableList<InstanceAttributeCellModel> getInstanceAttributeModels() {
		return instanceAttributesModels;
	}

	public void remove() {
		getGeneric().remove();
	}
}
