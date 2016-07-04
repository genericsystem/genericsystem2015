package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Tag.ModelConstructor;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

public interface CompositeElement<M extends GenericModel> {

	default StringExtractor getStringExtractor() {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	default ModelConstructor<GenericModel> getModelConstructor() {
		return GenericModel::new;
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<GenericModel> constructor);
}