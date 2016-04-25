package org.genericsystem.distributed.cacheonserver.ui.list;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TitleGenericCompositeModel extends GenericCompositeModel<GenericModel> {

	public TitleGenericCompositeModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(generic, observableListExtractor, GenericModel::new);
	}

	public TitleGenericCompositeModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, GenericModel> elementBuilder) {
		this(generic, g -> (GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management"), observableListExtractor, elementBuilder);
	}

	public TitleGenericCompositeModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, GenericModel> elementBuilder) {
		super(generic, stringExtractor, observableListExtractor, elementBuilder);
	}
}