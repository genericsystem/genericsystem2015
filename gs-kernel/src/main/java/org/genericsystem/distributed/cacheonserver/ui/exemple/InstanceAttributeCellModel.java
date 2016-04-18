package org.genericsystem.distributed.cacheonserver.ui.exemple;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model;

public class InstanceAttributeCellModel extends Model {

	private final Generic instance;
	private final Generic attribute;
	private final ObservableList<HolderSubCellModel> holderModels;

	public InstanceAttributeCellModel(Generic instance, Generic attribute) {
		this.instance = instance;
		this.attribute = attribute;
		holderModels = new Transformation2<>(instance.getObservableHolders(attribute), HolderSubCellModel::new /* , carModel -> new Observable[] { CarModel.() } */);
		System.out.println("coucou2");
	}

	public ObservableList<HolderSubCellModel> getHolderModels() {
		return holderModels;
	}
}
