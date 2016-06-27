package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Element.ModelConstructor;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor;

public interface CompositeElement<M extends CompositeModel> {

	default StringExtractor getStringExtractor() {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	default ModelConstructor<CompositeModel> getModelConstructor() {
		return CompositeModel::new;
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor);
}