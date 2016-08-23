package org.genericsystem.defaults.tools;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import com.sun.javafx.collections.ObservableListWrapper;

import javafx.beans.Observable;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.util.Callback;

public class ObservableListWrapperExtended<E> extends ObservableListWrapper<E> implements ListChangeListener<E> {

	private final BiConsumer<Integer, E> addBiConsumer;
	private final ObservableList<E> external;
	private final Consumer<Integer> removeConsumer;
	private BindingHelperObserver<E> observer;

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

	public BiConsumer<Integer, E> getAddBiConsumer() {
		return addBiConsumer;
	}

	public Consumer<Integer> getRemoveConsumer() {
		return removeConsumer;
	}

	public static class BindingHelperObserver<E> implements ListChangeListener<E> {

		private final WeakReference<ObservableListWrapperExtended<E>> ref;

		public BindingHelperObserver(ObservableListWrapperExtended<E> transformationList) {
			if (transformationList == null) {
				throw new NullPointerException("Binding has to be specified.");
			}
			ref = new WeakReference<ObservableListWrapperExtended<E>>(transformationList);
		}

		@Override
		public void onChanged(Change<? extends E> change) {
			final ObservableListWrapperExtended<E> binding = ref.get();
			if (binding == null) {
				change.getList().removeListener(this);
			} else {
				binding.onChanged(change);
			}

		}

	}

	public ObservableListWrapperExtended(ObservableList<E> external, Callback<E, Observable[]> extractor) {
		super(new ArrayList<>(), extractor);
		this.external = external; // prevents of listener garbage collection
		this.addBiConsumer = (index, src) -> add(index, src);
		this.removeConsumer = index -> remove(index.intValue());
		bind();
		int i = 0;
		beginChange();
		for (E element : external)
			getAddBiConsumer().accept(i++, element);
		endChange();
	}

	@Override
	public void onChanged(Change<? extends E> change) {
		System.out.println("------------- onChange dans ObservableListWrapperExtended");
		while (change.next()) {
			beginChange();
			if (change.wasPermutated()) {
				assert false;// Not tested after
				for (int i = change.getFrom(); i < change.getTo(); i++)
					getRemoveConsumer().accept(change.getFrom());
				int index = change.getFrom();
				for (E source : change.getList().subList(change.getFrom(), change.getTo()))
					getAddBiConsumer().accept(index++, source);
			} else {
				if (change.wasRemoved()) {
					for (int i = 0; i < change.getRemovedSize(); i++)
						getRemoveConsumer().accept(change.getFrom());
				}
				if (change.wasAdded()) {
					int index = change.getFrom();
					for (E source : change.getAddedSubList())
						getAddBiConsumer().accept(index++, source);
				}
			}
			endChange();
		}
	}
}
