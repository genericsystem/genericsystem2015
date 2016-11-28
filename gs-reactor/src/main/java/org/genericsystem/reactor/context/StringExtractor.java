package org.genericsystem.reactor.context;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;

@FunctionalInterface
public interface StringExtractor extends Function<Generic, String> {
	public static final StringExtractor EXTRACTOR = generic -> generic != null ? Objects.toString(generic.getValue()) : "";
	public static final StringExtractor SIMPLE_CLASS_EXTRACTOR = generic -> {
		if (generic == null)
			return "";
		Serializable value = generic.getValue();
		if (value instanceof Class)
			return ((Class<?>) value).getSimpleName();
		if (value != null && value.getClass().isArray()) {
			if (int[].class.equals(value.getClass()))
				return Arrays.stream((int[]) value).boxed().collect(Collectors.toList()).toString();
			return Arrays.toString((Object[]) value);
		}
		return Objects.toString(value);
	};

	public static final StringExtractor MANAGEMENT = g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management";
	public static final StringExtractor TYPE_INSTANCE_EXTRACTOR = generic -> {
		return "(" + SIMPLE_CLASS_EXTRACTOR.apply(generic.getMeta()) + ") " + SIMPLE_CLASS_EXTRACTOR.apply(generic);
	};
	public static final StringExtractor INFO = Generic::info;

	public static class EXTRACTOR implements StringExtractor {
		@Override
		public String apply(Generic generic) {
			return EXTRACTOR.apply(generic);
		}
	}

	public static class SIMPLE_CLASS_EXTRACTOR implements StringExtractor {
		@Override
		public String apply(Generic generic) {
			return SIMPLE_CLASS_EXTRACTOR.apply(generic);
		}
	}

	public static class MANAGEMENT implements StringExtractor {
		@Override
		public String apply(Generic generic) {
			return MANAGEMENT.apply(generic);
		}
	}

	public static class TYPE_INSTANCE_EXTRACTOR implements StringExtractor {
		@Override
		public String apply(Generic generic) {
			return TYPE_INSTANCE_EXTRACTOR.apply(generic);
		}
	}

	public static class INFO implements StringExtractor {
		@Override
		public String apply(Generic generic) {
			return INFO.apply(generic);
		}
	}
}