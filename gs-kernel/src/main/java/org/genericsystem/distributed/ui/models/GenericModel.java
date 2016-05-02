package org.genericsystem.distributed.ui.models;

import java.io.Serializable;
import java.util.Objects;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class GenericModel extends CompositeModel<Model> {

	public static final StringExtractor EXTRACTOR = generic -> Objects.toString(generic.getValue());
	public static final StringExtractor SIMPLE_CLASS_EXTRACTOR = generic -> {
		Serializable value = generic.getValue();
		return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
	};

	public GenericModel(Generic... generics) {
		this(generics, SIMPLE_CLASS_EXTRACTOR);
	}

	public GenericModel(Generic[] generics, StringExtractor stringExtractor) {
		this(generics, stringExtractor, null, null);
	}

	public GenericModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
	}
}
