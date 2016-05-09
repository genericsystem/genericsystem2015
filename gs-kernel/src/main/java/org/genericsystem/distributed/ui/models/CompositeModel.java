package org.genericsystem.distributed.ui.models;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
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
	private ObservableList<M> subModels;

	public <C extends CompositeModel<M>> C initSubModels(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<M> constructor) {
		return initSubModels(observableListExtractor, gs -> constructor.build(gs, stringExtractor));
	}

	public <C extends CompositeModel<M>> C initSubModels(ObservableListExtractor observableListExtractor, Builder<M> leafBuilder) {
		return initSubModels(new Transformation2<>(observableListExtractor.apply(generics), generic -> leafBuilder.apply(CompositeModel.addToGenerics(generic, generics))));
	}

	public <C extends CompositeModel<M>> C initSubModels(ObservableList<M> subModels) {
		assert this.subModels == null;
		this.subModels = subModels;
		return (C) this;
	}

	public ObservableList<M> getSubModels() {
		return subModels;
	}

	public ObservableValue<M> getFirstSubModel() {
		return Bindings.valueAt(subModels, 0);
	}

	public ObservableValue<M> getSecondSubModel() {
		return Bindings.valueAt(subModels, 1);
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor) {
		assert stringExtractor != null;
		this.generics = generics;
		this.stringExtractor = stringExtractor;
	}

	private Generic[] getGenerics() {
		return generics;
	}

	public static Generic[] addToGenerics(Generic generic, Generic[] generics) {
		Generic[] result = new Generic[generics.length + 1];
		result[0] = generic;
		System.arraycopy(generics, 0, result, 1, generics.length);
		return result;
	}

	public Generic getGeneric() {
		return generics[0];
	}

	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(stringExtractor.apply(getGenerics()[0]));
	}

	public void remove() {
		getGeneric().remove();
	}

	@FunctionalInterface
	public static interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {

	}

	@FunctionalInterface
	public static interface StringExtractor extends Function<Generic, String> {
		public static final StringExtractor EXTRACTOR = generic -> Objects.toString(generic.getValue());
		public static final StringExtractor SIMPLE_CLASS_EXTRACTOR = generic -> {
			Serializable value = generic.getValue();
			return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
		};
	}

	@FunctionalInterface
	public static interface Builder<M extends Model> extends Function<Generic[], M> {

	}

	@FunctionalInterface
	public interface ModelConstructor<M extends Model> {
		M build(Generic[] generics, StringExtractor stringExtractor);
	}

}
