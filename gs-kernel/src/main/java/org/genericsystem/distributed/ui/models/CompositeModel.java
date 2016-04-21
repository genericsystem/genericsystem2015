package org.genericsystem.distributed.ui.models;

import javafx.collections.ObservableList;
import org.genericsystem.distributed.ui.Model;

public class CompositeModel<T extends Model> extends Model {
	private final ObservableList<T> subModels;

	public CompositeModel(ObservableList<T> subModels) {
		this.subModels = subModels;
	}

	public ObservableList<T> getSubModels() {
		return subModels;
	}
}
