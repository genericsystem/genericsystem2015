package org.genericsystem.distributed.ui.models;

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

	private final ObservableList<M> subModels;

	public ObservableList<M> getSubModels() {
		return subModels;
	}

	public ObservableValue<M> getFirstSubModel() {
		return Bindings.valueAt(subModels, 0);
	}

	public ObservableValue<M> getSecondSubModel() {
		return Bindings.valueAt(subModels, 1);
	}

	public CompositeModel(Generic[] generics, ObservableListExtractor observableListExtractor) {
		this(generics, GenericModel.SIMPLE_CLASS_EXTRACTOR, observableListExtractor);
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		this(generics, stringExtractor, observableListExtractor, GenericModel::new);
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		this(generics, stringExtractor, new Transformation2<>(observableListExtractor.apply(generics), generic -> (M) builder.apply(addToGenerics(generic, generics))));
	}

	public CompositeModel(Generic[] generics, StringExtractor stringExtractor, ObservableList<M> subModels) {
		assert stringExtractor != null;
		this.generics = generics;
		this.stringExtractor = stringExtractor;
		this.subModels = subModels;
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

	@FunctionalInterface
	public static interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {

	}

	@FunctionalInterface
	public static interface StringExtractor extends Function<Generic, String> {

	}

	@FunctionalInterface
	public static interface Builder<M extends CompositeModel<?>> extends Function<Generic[], M> {

	}

	// @FunctionalInterface
	// public interface CompositeCdonstructor<M extends CompositeModel<?>> {
	// M build(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder);
	// }

	@FunctionalInterface
	public interface CompositeGenericConstructor<M extends CompositeModel<?>, SUBMODEL extends Model> {
		M build(Generic[] generics, StringExtractor stringExtractor, ObservableList<SUBMODEL> subModels);
	}

}
