package org.genericsystem.distributed.ui;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class CompositeModel extends Model {

	private final Generic[] generics;
	private final StringExtractor stringExtractor;
	private ObservableList<CompositeModel> subModels;

	public <C extends CompositeModel> C initSubModels(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		return initSubModels(observableListExtractor, gs -> constructor.build(gs, stringExtractor));
	}

	public <C extends CompositeModel> C initSubModels(ObservableListExtractor observableListExtractor, Builder<CompositeModel> leafBuilder) {
		return initSubModels(new Transformation2<>(observableListExtractor.apply(generics), generic -> leafBuilder.apply(CompositeModel.addToGenerics(generic, generics))));
	}

	public <C extends CompositeModel> C initSubModels(ObservableList<CompositeModel> subModels) {
		this.subModels = subModels;
		return (C) this;
	}

	public ObservableList<CompositeModel> getSubModels() {
		return subModels;
	}

	public ObservableValue<CompositeModel> getFirstSubModel() {
		return Bindings.valueAt(subModels, 0);
	}

	public ObservableValue<CompositeModel> getSecondSubModel() {
		return Bindings.valueAt(subModels, 1);
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor) {
		assert stringExtractor != null;
		this.generics = generics;
		this.stringExtractor = stringExtractor;
	}

	Generic[] getGenerics() {
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
		public static final ObservableListExtractor INSTANCES = generics -> {
			System.out.println("INSTANCES : " + Arrays.toString(generics));
			return generics[0].getObservableSubInstances();
		};

		public static final ObservableListExtractor ATTRIBUTES = gs -> gs[0].getObservableAttributes();
		public static final ObservableListExtractor HOLDERS = generics -> {
			System.out.println("HOLDERS : " + Arrays.toString(generics));
			return generics[1].getObservableHolders(generics[0]);
		};
	}

	@FunctionalInterface
	public static interface StringExtractor extends Function<Generic, String> {
		public static final StringExtractor EXTRACTOR = generic -> Objects.toString(generic.getValue());
		public static final StringExtractor SIMPLE_CLASS_EXTRACTOR = generic -> {
			Serializable value = generic.getValue();
			return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
		};
		public static final StringExtractor MANAGEMENT = g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management";
	}

	@FunctionalInterface
	public static interface Builder<M extends Model> extends Function<Generic[], M> {

	}

	@FunctionalInterface
	public interface ModelConstructor<M extends Model> {
		M build(Generic[] generics, StringExtractor stringExtractor);
	}

}
