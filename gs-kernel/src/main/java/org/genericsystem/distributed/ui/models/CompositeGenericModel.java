package org.genericsystem.distributed.ui.models;

import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class CompositeGenericModel<T extends Model> extends GenericModel {
	private final ObservableList<T> subModels;

	public CompositeGenericModel(Generic generic, ObservableList<T> subModels) {
		super(generic);
		this.subModels = subModels;
	}

	public ObservableList<T> getSubModels() {
		return subModels;
	}
}
