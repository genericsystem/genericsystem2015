package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TitleGenericCompositeModel extends CompositeModel<GenericModel> {

	public TitleGenericCompositeModel(Generic[] generics, ObservableListExtractor observableListExtractor) {
		this(generics, GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
	}

	public TitleGenericCompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		this(generics, stringExtractor, observableListExtractor, GenericModel::new);
	}

	public TitleGenericCompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
	}
}
