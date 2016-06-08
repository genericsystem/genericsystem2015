package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;

public interface Composite<M extends CompositeModel> {

	default StringExtractor getStringExtractor() {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	@SuppressWarnings("unchecked")
	default ModelConstructor<M> getModelConstructor() {
		return (ModelConstructor) CompositeModel::new;
	}
}