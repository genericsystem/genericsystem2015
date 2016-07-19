package org.genericsystem.defaults.tools;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

import com.sun.javafx.collections.ObservableListWrapper;

import javafx.beans.Observable;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.util.Callback;

/**
 * @author Nicolas Feybesse
 *
 * @param <SOURCE>
 * @param <TARGET>
 */

public class TransformationObservableList<SOURCE, TARGET> extends ObservableListWrapper<TARGET> implements ListChangeListener<SOURCE> {

	private final BiConsumer<Integer, SOURCE> addBiConsumer;
	private final ObservableList<SOURCE> external;
	private final Consumer<Integer> removeConsumer;
	private BindingHelperObserver<SOURCE> observer;

	public final void bind() {
		if (observer == null)
			observer = new BindingHelperObserver<>(this);
		external.addListener(observer);
	}

	public final void unbind() {
		if (observer != null)
			external.removeListener(observer);
		observer = null;
	}

	public BiConsumer<Integer, SOURCE> getAddBiConsumer() {
		return addBiConsumer;
	}

	public Consumer<Integer> getRemoveConsumer() {
		return removeConsumer;
	}

	public static class BindingHelperObserver<E> implements ListChangeListener<E> {

		private final WeakReference<TransformationObservableList<E, ?>> ref;

		public BindingHelperObserver(TransformationObservableList<E, ?> transformationList) {
			if (transformationList == null) {
				throw new NullPointerException("Binding has to be specified.");
			}
			ref = new WeakReference<TransformationObservableList<E, ?>>(transformationList);
		}

		@Override
		public void onChanged(Change<? extends E> change) {
			final TransformationObservableList<E, ?> binding = ref.get();
			if (binding == null) {
				change.getList().removeListener(this);
			} else {
				binding.onChanged(change);
			}

		}

	}

	// private final ListChangeListener<SOURCE> listener = change -> {
	// while (change.next()) {
	// beginChange();
	// if (change.wasPermutated()) {
	// assert false;// Not tested after
	// for (int i = change.getFrom(); i < change.getTo(); i++)
	// getRemoveConsumer().accept(change.getFrom());
	// int index = change.getFrom();
	// for (SOURCE source : change.getList().subList(change.getFrom(), change.getTo()))
	// getAddBiConsumer().accept(index++, source);
	// } else {
	// if (change.wasRemoved()) {
	// for (int i = 0; i < change.getRemovedSize(); i++)
	// getRemoveConsumer().accept(change.getFrom());
	// }
	// if (change.wasAdded()) {
	// int index = change.getFrom();
	// for (SOURCE source : change.getAddedSubList())
	// getAddBiConsumer().accept(index++, source);
	// }
	// }
	// endChange();
	// }
	// };

	public TransformationObservableList(ObservableList<SOURCE> external, Function<SOURCE, TARGET> srcToTarget) {
		super(new ArrayList<>());
		this.external = external; // prevent of listener garbage collection
		this.addBiConsumer = (index, src) -> add(index, srcToTarget.apply(src));
		this.removeConsumer = index -> remove(index.intValue());
		bind();
		int i = 0;
		beginChange();
		for (SOURCE element : external)
			getAddBiConsumer().accept(i++, element);
		endChange();
	}

	public TransformationObservableList(ObservableList<SOURCE> external, Function<SOURCE, TARGET> srcToTarget, Callback<TARGET, Observable[]> extractor) {
		super(new ArrayList<>(), extractor);
		this.external = external; // prevents of listener garbage collection
		this.addBiConsumer = (index, src) -> add(index, srcToTarget.apply(src));
		this.removeConsumer = index -> remove(index.intValue());
		bind();
		int i = 0;
		beginChange();
		for (SOURCE element : external)
			getAddBiConsumer().accept(i++, element);
		endChange();
	}

	public TransformationObservableList(ObservableList<SOURCE> external, BiFunction<Integer, SOURCE, TARGET> add, Consumer<TARGET> consumer) {
		super(new ArrayList<>());
		this.external = external; // prevents of listener garbage collection
		this.addBiConsumer = (index, src) -> add(index, add.apply(index, src));
		this.removeConsumer = index -> consumer.accept(remove(index.intValue()));
		bind();
		int i = 0;
		beginChange();
		for (SOURCE element : external)
			getAddBiConsumer().accept(i++, element);
		endChange();
	}

	@Override
	public void onChanged(Change<? extends SOURCE> change) {
		while (change.next()) {
			beginChange();
			if (change.wasPermutated()) {
				assert false;// Not tested after
				for (int i = change.getFrom(); i < change.getTo(); i++)
					getRemoveConsumer().accept(change.getFrom());
				int index = change.getFrom();
				for (SOURCE source : change.getList().subList(change.getFrom(), change.getTo()))
					getAddBiConsumer().accept(index++, source);
			} else {
				if (change.wasRemoved()) {
					for (int i = 0; i < change.getRemovedSize(); i++)
						getRemoveConsumer().accept(change.getFrom());
				}
				if (change.wasAdded()) {
					int index = change.getFrom();
					for (SOURCE source : change.getAddedSubList())
						getAddBiConsumer().accept(index++, source);
				}
			}
			endChange();
		}
	}

}
