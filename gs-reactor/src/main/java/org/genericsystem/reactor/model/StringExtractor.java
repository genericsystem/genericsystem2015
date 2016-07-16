package org.genericsystem.reactor.model;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import org.genericsystem.common.Generic;

@FunctionalInterface
public interface StringExtractor extends Function<Generic, String> {
	public static final StringExtractor EXTRACTOR = generic -> generic != null ? Objects.toString(generic.getValue()) : "";
	public static final StringExtractor SIMPLE_CLASS_EXTRACTOR = generic -> {
		if (generic == null)
			return "";
		Serializable value = generic.getValue();
		return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
	};
	public static final StringExtractor MANAGEMENT = g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management";
	public static final StringExtractor TYPE_INSTANCE_EXTRACTOR = generic -> {
		return "(" + SIMPLE_CLASS_EXTRACTOR.apply(generic.getMeta()) + ") " + SIMPLE_CLASS_EXTRACTOR.apply(generic);
	};
	public static final StringExtractor INFO = Generic::info;
}