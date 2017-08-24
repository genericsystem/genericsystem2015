package org.genericsystem.defaults.tools;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import javafx.util.Callback;

/**
 * @author Nicolas Feybesse
 *
 */
public class BindingsTools {

	public static ObservableValue<?> create(ObservableValue<?>... observables) {
		return Bindings.createObjectBinding(() -> Arrays.asList(observables).stream().map(o -> o.getValue()).toArray(Object[]::new), observables);
	}

	public static <T> ObservableValue<?> createTransitive(ObservableValue<T> master, Function<T, ObservableValue<?>[]> extractor) {
		ObservableValue<?> invalidator = new ObjectBinding<ObservableValue<?>>() {
			private ObservableList<ObservableValue<?>> value = FXCollections.observableArrayList(o -> new Observable[]{ o });
			{
				bind(master);
				bind(value);
			}

			@Override
			protected ObservableValue<?> computeValue() {
				value.clear();
				value.addAll(extractor.apply(master.getValue()));
				return value.isEmpty() ? null : value.get(0);
			}
		};
		return invalidator;
	}

	public static <E> ObservableList<E> createMinimalUnitaryChangesBinding(ObservableList<E> source) {
		return new ListBinding<E>() {
			private ObservableList<E> internal = FXCollections.observableArrayList();

			private ListChangeListener<E> onSrcInvalidation = (c) -> MinimalUnitaryChanges.doMinimalChanges(source, internal);
			{
				source.addListener(new WeakListChangeListener<>(onSrcInvalidation));
				MinimalUnitaryChanges.doMinimalChanges(source, internal);
			}

			@Override
			protected ObservableList<E> computeValue() {
				return internal;
			}
		};
	}

	public static <E> ObservableList<E> createMinimalUnitaryChangesBinding(ObservableList<E> source, Supplier<List<E>> subElements, Callback<E, Observable[]> extractor) {
		return createMinimalUnitaryChangesBinding(new ListBinding<E>() {
			private Observable invalidator = new ObservableListWrapper<E>(source, extractor);
			{
				bind(invalidator);
			}

			@Override
			protected ObservableList<E> computeValue() {
				return FXCollections.observableList(subElements.get());
			};
		});
	}
}
