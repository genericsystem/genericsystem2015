package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

/**
 * @author Nicolas Feybesse
 *
 */
public class BindingsTools {

	public static ObservableValue<?> create(ObservableValue<?>... observables) {
		return Bindings.createObjectBinding(() -> "", observables);
	}

	public static <T> ObservableValue<?> createTransitive(ObservableValue<T> master, Function<T, ObservableValue<?>[]> slavesFromValue) {
		ObservableValue<?> result = new ObjectBinding<Object>() {
			private ObservableValue<?>[] slaves;
			private ChangeListener<T> onMasterValueChange = (o, v, nv) -> onMasterValueChange();

			{
				master.addListener(new WeakChangeListener<T>(onMasterValueChange));
				onMasterValueChange();
			}

			private void onMasterValueChange() {
				unbind(slaves);
				invalidate();
				bind(slaves = slavesFromValue.apply(master.getValue()));
			}

			@Override
			protected void onInvalidating() {
				// Force reevaluation of the binding so that the event is transmitted to Observables listening on its changes.
				getValue();
			}

			@Override
			protected Object computeValue() {
				return Stream.concat(Stream.of(master.getValue()), Arrays.asList(slaves).stream().map(o -> o.getValue())).toArray(Object[]::new);
			}
		};
		return result;
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

	public static <E> ObservableList<E> createMinimalUnitaryChangesBinding(ObservableList<E> source, Supplier<List<E>> subElements, Function<E, Observable> slaveSupplier) {
		return createMinimalUnitaryChangesBinding(new ListBinding<E>() {
			private List<Observable> slaveInvalidators = new ArrayList<>();
			private ListChangeListener<E> onSrcInvalidation = (c) -> onSrcInvalidation();
			{
				source.addListener(new WeakListChangeListener<>(onSrcInvalidation));
				onSrcInvalidation();
			}

			@Override
			protected ObservableList<E> computeValue() {
				return FXCollections.observableList(subElements.get());
			}

			void onSrcInvalidation() {
				slaveInvalidators.forEach(this::unbind);
				slaveInvalidators.clear();
				invalidate();
				source.forEach(e -> slaveInvalidators.add(slaveSupplier.apply(e)));
				slaveInvalidators.forEach(this::bind);
			}

		});
	}

}
