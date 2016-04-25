package org.genericsystem.distributed.ui.models;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class GenericCompositeModel<M extends Model> extends CompositeModel<M> {

	private final Generic generic;
	private final Function<Generic, String> stringExtractor;

	public GenericCompositeModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
		this(generic, GenericModel.EXTRACTOR, observableListExtractor, elementBuilder);
	}

	public GenericCompositeModel(Generic generic, Function<Generic, String> stringExtractor, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, M> elementBuilder) {
		super(generic, observableListExtractor, elementBuilder);
		this.generic = generic;
		this.stringExtractor = stringExtractor;
	}

	protected Generic getGeneric() {
		return generic;
	}

	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(stringExtractor.apply(generic));
	}
}
