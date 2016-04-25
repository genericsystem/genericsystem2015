package org.genericsystem.distributed.ui.models;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyStringWrapper;

import org.genericsystem.common.Generic;

public class GenericModel extends StringModel {

	public static final Function<Generic, String> EXTRACTOR = generic -> Objects.toString(generic.getValue());
	public static final Function<Generic, String> SIMPLE_CLASS_EXTRACTOR = generic -> {
		Serializable value = generic.getValue();
		return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
	};
	private final Generic generic;

	public GenericModel(Generic generic) {
		this(generic, EXTRACTOR);
	}

	public GenericModel(Generic generic, Function<Generic, String> stringExtractor) {
		super(new ReadOnlyStringWrapper(stringExtractor.apply(generic)));
		this.generic = generic;
	}

	public Generic getGeneric() {
		return generic;
	}

}
