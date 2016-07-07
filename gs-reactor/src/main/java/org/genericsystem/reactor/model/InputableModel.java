package org.genericsystem.reactor.model;

import java.io.Serializable;

import org.genericsystem.common.Generic;

import javafx.beans.property.Property;

public interface InputableModel {
	abstract Serializable getValue();

	abstract Property<TriFunction<Generic[], Serializable, Generic, Generic>> getInputAction();

	@FunctionalInterface
	public interface TriFunction<T, U, R, S> {
		R apply(T t, U u, S s);
	}
}
