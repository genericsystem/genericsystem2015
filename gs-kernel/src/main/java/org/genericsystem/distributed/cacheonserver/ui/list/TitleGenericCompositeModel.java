package org.genericsystem.distributed.cacheonserver.ui.list;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TitleGenericCompositeModel extends CompositeModel<GenericModel> {

	public TitleGenericCompositeModel(Conf compositeConf) {
		super(compositeConf);
	}

	public TitleGenericCompositeModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor) {
		this(new Conf(generic, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", observableListExtractor, GenericModel::new));
	}

	public TitleGenericCompositeModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, GenericModel> elementBuilder) {
		this(new Conf(generic, g -> GenericModel.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management", observableListExtractor, elementBuilder));
	}
}