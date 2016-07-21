package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public interface CompositeTag {

	default StringExtractor getStringExtractor() {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.SUBINSTANCES;
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor);
}