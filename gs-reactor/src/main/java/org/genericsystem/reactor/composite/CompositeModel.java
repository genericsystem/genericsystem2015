package org.genericsystem.reactor.composite;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.reactor.Model;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
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

	public Generic[] getGenerics() {
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
	public static interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {

		public static ObservableListExtractor from(Class<?>... genericClasses) {
			return gs -> FXCollections.observableArrayList(Arrays.stream(genericClasses).map(gs[0].getRoot()::<Generic> find).collect(Collectors.toList()));
		}

		public static final ObservableListExtractor INSTANCES = generics -> {
			System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableInstances());
			return generics[0].getObservableSubInstances();
		};

		public static final ObservableListExtractor SUBINSTANCES = generics -> {
			System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableSubInstances());
			return generics[0].getObservableSubInstances();
		};

		public static final ObservableListExtractor ATTRIBUTES_OF_TYPE = generics -> {
			System.out.println("ATTRIBUTES_OF_TYPE : " + Arrays.toString(generics) + " "
					+ generics[0].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[0])));
			return generics[0].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[0]));
		};

		public static final ObservableListExtractor ATTRIBUTES_OF_INSTANCES = generics -> {
			System.out.println("ATTRIBUTES_OF_INSTANCES : " + Arrays.toString(generics) + " "
					+ generics[1].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[1])));
			return generics[1].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[1]));
		};

		public static final ObservableListExtractor COMPONENTS = generics -> {
			System.out.println("COMPONENTS : " + Arrays.toString(generics) + " " + generics[0].getComponents());
			return FXCollections.observableList(generics[0].getComponents());
		};

		public static final ObservableListExtractor HOLDERS = generics -> {
			System.out.println("HOLDERS : " + Arrays.toString(generics) + " " + generics[1].getObservableHolders(generics[0]));
			return generics[1].getObservableHolders(generics[0]);
		};
	}

	@FunctionalInterface
	public static interface StringExtractor extends Function<Generic, String> {
		public static final StringExtractor EXTRACTOR = generic -> generic != null ? Objects.toString(generic.getValue()) : "";
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

	private Set<ObservableList<?>> observableLists = new HashSet<ObservableList<?>>();

	public <M extends Model> ObservableList<M> getObservableList(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor,
			ModelConstructor<CompositeModel> constructor) {
		ObservableList<M> observableList = new Transformation2<Generic, M>(observableListExtractor.apply(generics),
				generic -> (M) constructor.build(CompositeModel.addToGenerics(generic, generics), stringExtractor));
		observableLists.add(observableList);// Prevents garbaging
		return observableList;
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}

	public static class InputCompositeModel extends CompositeModel {
		private Property<String> inputString = new SimpleStringProperty();

		public InputCompositeModel(Generic[] generics, StringExtractor extractor) {
			super(generics, extractor);
		}

		public Property<String> getInputString() {
			return inputString;
		}
	}

	public static class SelectorModel extends CompositeModel {
		private Property<Generic> selection = new SimpleObjectProperty<>();
		private ObservableValue<String> selectionString = Bindings.createStringBinding(() -> getStringExtractor().apply(getSelection().getValue()),
				getSelection());

		public SelectorModel(Generic[] generics, StringExtractor extractor) {
			super(generics, extractor);
		}

		public Property<Generic> getSelection() {
			return selection;
		}

		public ObservableValue<String> getSelectionString() {
			return selectionString;
		}
	}

}
