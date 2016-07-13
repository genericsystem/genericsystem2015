package org.genericsystem.reactor.model;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GenericModel extends Model {

	private final Generic[] generics;
	private final StringExtractor stringExtractor;
	private final Property<GenericModel> selection = new SimpleObjectProperty<GenericModel>();
	private final ObservableValue<String> selectionString = Bindings.createStringBinding(() -> getStringExtractor().apply(getSelection().getValue() != null ? getSelection().getValue().getGeneric() : null), getSelection());
	private boolean selector = false;

	public GenericModel(Generic[] generics, StringExtractor stringExtractor) {
		assert stringExtractor != null;
		this.generics = generics;
		this.stringExtractor = stringExtractor;
	}

	public GenericModel(Model parent, Generic[] generics, StringExtractor stringExtractor) {
		this(generics, stringExtractor);
		this.parent = parent;
	}

	public Generic[] getGenerics() {
		return generics;
	}

	public Property<GenericModel> getSelection() {
		return selection;
	}

	public ObservableValue<String> getSelectionString() {
		return selectionString;
	}

	public boolean isSelector() {
		return selector;
	}

	public void markSelector() {
		this.selector = true;
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

	// TODO KK no cache ?
	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(stringExtractor.apply(getGenerics()[0]));
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	public void remove() {
		System.out.println("remove!!!");
		getGeneric().remove();
	}

	public void select() {
		System.out.println("select click!!!");
	}

	@FunctionalInterface
	public static interface StringExtractor extends Function<Generic, String> {
		public static final StringExtractor EXTRACTOR = generic -> generic != null ? Objects.toString(generic.getValue()) : "";
		public static final StringExtractor SIMPLE_CLASS_EXTRACTOR = generic -> {
			Serializable value = generic.getValue();
			return value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value);
		};
		public static final StringExtractor MANAGEMENT = g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management";
		public static final StringExtractor TYPE_INSTANCE_EXTRACTOR = generic -> {
			return "(" + SIMPLE_CLASS_EXTRACTOR.apply(generic.getMeta()) + ") " + SIMPLE_CLASS_EXTRACTOR.apply(generic);
		};
		public static final StringExtractor INFO = Generic::info;
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}
}
