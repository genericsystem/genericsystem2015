package org.genericsystem.distributed.ui.models;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class CompositeModel<M extends Model> extends Model {

	private final Generic[] generics;
	private final StringExtractor stringExtractor;
	private final ObservableListExtractor observableListExtractor;
	private final Builder<?> builder;

	private final ObservableList<M> subModels;

	public ObservableList<M> getSubModels() {
		return subModels;
	}

	public CompositeModel(Generic[] generics, ObservableListExtractor observableListExtractor) {
		this(generics, GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		this(generics, stringExtractor, observableListExtractor, GenericModel::new);
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		assert stringExtractor != null;
		this.generics = generics;
		this.stringExtractor = stringExtractor;
		this.observableListExtractor = observableListExtractor;
		this.builder = builder;
		this.subModels = observableListExtractor != null ? new Transformation2<>(observableListExtractor.apply(getGenerics()), generic -> (M) builder.apply(addToGenerics(generic))) : null;
	}

	private Generic[] getGenerics() {
		return generics;
	}

	private Generic[] addToGenerics(Generic generic) {
		Generic[] result = new Generic[getGenerics().length + 1];
		result[0] = generic;
		System.arraycopy(getGenerics(), 0, result, 1, getGenerics().length);
		return result;
	}

	public Generic getGeneric() {
		return generics[0];
	}

	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(stringExtractor.apply(getGenerics()[0]));
	}

	@FunctionalInterface
	public static interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {

	}

	@FunctionalInterface
	public static interface StringExtractor extends Function<Generic, String> {

	}

	@FunctionalInterface
	public static interface Builder<M extends CompositeModel<?>> extends Function<Generic[], M> {

	}
}
