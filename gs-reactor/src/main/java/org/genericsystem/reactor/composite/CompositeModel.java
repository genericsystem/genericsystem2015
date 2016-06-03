package org.genericsystem.reactor.composite;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class CompositeModel extends Model {

	private final Generic[] generics;
	private final StringExtractor stringExtractor;

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

	public void select() {
		System.out.println("select click!!!");
	}

	@FunctionalInterface
	public static interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {
		public static final ObservableListExtractor INSTANCES = generics -> {
			System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableSubInstances());
			return generics[0].getObservableSubInstances();
		};

		public static final ObservableListExtractor ATTRIBUTES = generics -> {
			System.out.println("ATTRIBUTES : " + Arrays.toString(generics) + " " + generics[0].getObservableAttributes());
			return generics[0].getObservableAttributes();
		};
		public static final ObservableListExtractor HOLDERS = generics -> {
			System.out.println("HOLDERS : " + Arrays.toString(generics) + " " + generics[1].getObservableHolders(generics[0]));
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

	private Map<Element<?, ?>, ObservableList<CompositeModel>> observableLists = new HashMap<Element<?, ?>, ObservableList<CompositeModel>>();

	public <M extends CompositeModel> ObservableList<CompositeModel> getBoundObservableList(Element<?, ?> element, StringExtractor stringExtractor,
			ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		ObservableList<CompositeModel> observableList = new Transformation2<Generic, CompositeModel>(observableListExtractor.apply(generics),
				generic -> constructor.build(CompositeModel.addToGenerics(generic, generics), stringExtractor));
		observableLists.put(element, observableList);// Prevents garbaging
		return observableList;
	}

	public <M extends CompositeModel> ObservableList<CompositeModel> getBoundObservableList(Element<?, ?> element, StringExtractor stringExtractor,
			Supplier<Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		return getBoundObservableList(element, stringExtractor, gs -> FXCollections.singletonObservableList(genericSupplier.get()), constructor);
	}

	public <M extends CompositeModel> ObservableList<CompositeModel> getBoundObservableList(Element<?, ?> element, StringExtractor stringExtractor,
			Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		return getBoundObservableList(element, stringExtractor, () -> getGenerics()[0].getRoot().find(genericClass), constructor);
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

}
