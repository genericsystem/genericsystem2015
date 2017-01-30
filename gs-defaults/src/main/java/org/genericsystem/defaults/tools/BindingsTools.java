package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.binding.Binding;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

/**
 * @author Nicolas Feybesse
 *
 */
public class BindingsTools {
	public static <U extends Binding<V>, V> U transmitSuccessiveInvalidations(U binding) {
		binding.addListener((o, v, nv) -> {
		});
		return binding;
	}

	public static <T> Observable create(Observable... observables) {
		return transmitSuccessiveInvalidations(Bindings.createObjectBinding(() -> "", observables));
	}

	public static <T> Observable createTransitive(ObservableValue<T> master, Function<T, Observable[]> slavesFromValue) {
		return transmitSuccessiveInvalidations(new ObjectBinding<Object>() {
			private Observable[] slaves;
			private InvalidationListener onMasterInvalidion = (c) -> onMasterInvalidation();

			{
				master.addListener(new WeakInvalidationListener(onMasterInvalidion));
				onMasterInvalidation();
			}

			private void onMasterInvalidation() {
				unbind(slaves);
				invalidate();
				bind(slaves = slavesFromValue.apply(master.getValue()));
			}

			@Override
			protected Object computeValue() {
				return "";
			}
		});
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
