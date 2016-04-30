package org.genericsystem.distributed.ui.models;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class GenericModel extends GenericCompositeModel<Model> {

	public static final Function<Generic, String> EXTRACTOR = generic -> Objects.toString(generic.getValue());
	public static final Function<Generic, String> SIMPLE_CLASS_EXTRACTOR = generic -> {
		Serializable value = generic.getValue();
		return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
	};

	public GenericModel(Generic[] generics) {
		this(generics, EXTRACTOR);
	}

	public GenericModel(Generic[] generics, Function<Generic, String> stringExtractor) {
		this(new CompositeConf<>(generics, stringExtractor, null, null));
	}

	public GenericModel(CompositeConf<Model> conf) {
		super(conf);
	}
}
