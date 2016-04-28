package org.genericsystem.distributed.ui.models;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model;

public class TransformationModel<T extends Model> extends Model {
	private final ObservableList<T> subModels;

	protected <G> TransformationModel(ObservableList<G> genericComposites, Function<G, T> elementBuilder) {
		this(new Transformation2<>(genericComposites, elementBuilder));
	}

	public TransformationModel(ObservableList<T> subModels) {
		this.subModels = subModels;
	}

	public ObservableList<T> getSubModels() {
		return subModels;
	}
}
