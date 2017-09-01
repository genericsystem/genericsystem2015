package org.genericsystem.defaults.tools;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.RandomAccess;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import com.sun.javafx.collections.NonIterableChange;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.collections.ListChangeListener;
import javafx.collections.ModifiableObservableListBase;
import javafx.collections.ObservableList;
import javafx.util.Callback;

public class ObservableListWrapper<E> extends ModifiableObservableListBase<E>
implements ObservableList<E>, RandomAccess, ListChangeListener<E> {

	private final List<E> backingList;
	private final ElementObserver<E> elementObserver;

	private final BiConsumer<Integer, E> addBiConsumer;
	private final ObservableList<E> external;
	private final Consumer<Integer> removeConsumer;
	private BindingHelperObserver<E> observer = new BindingHelperObserver<>(this);

	public ObservableListWrapper(ObservableList<E> external, Callback<E, Observable[]> extractor) {
		backingList = new ArrayList<>();
		this.elementObserver = new ElementObserver<>(extractor, e -> o -> {
			beginChange();
			int i = 0;
			final int size = size();
			for (; i < size; ++i) {
				if (get(i) == e) {
					nextUpdate(i);
				}
			}
			endChange();
		});

		final int sz = backingList.size();
		for (int i = 0; i < sz; ++i) {
			elementObserver.attachListener(backingList.get(i));
		}

		this.external = external; // prevents of listener garbage collection
		this.addBiConsumer = (index, src) -> add(index, src);
		this.removeConsumer = index -> remove(index.intValue());
		external.addListener(observer);
		int i = 0;
		beginChange();
		for (E element : external)
			getAddBiConsumer().accept(i++, element);
		endChange();
	}

	public BiConsumer<Integer, E> getAddBiConsumer() {
		return addBiConsumer;
	}

	public Consumer<Integer> getRemoveConsumer() {
		return removeConsumer;
	}

	@Override
	public E get(int index) {
		return backingList.get(index);
	}

	@Override
	public int size() {
		return backingList.size();
	}

	@Override
	protected void doAdd(int index, E element) {
		if (elementObserver != null)
			elementObserver.attachListener(element);
		backingList.add(index, element);
	}

	@Override
	protected E doSet(int index, E element) {
		E removed = backingList.set(index, element);
		if (elementObserver != null) {
			elementObserver.detachListener(removed);
			elementObserver.attachListener(element);
		}
		return removed;
	}

	@Override
	protected E doRemove(int index) {
		E removed = backingList.remove(index);
		if (elementObserver != null)
			elementObserver.detachListener(removed);
		return removed;
	}

	@Override
	public int indexOf(Object o) {
		return backingList.indexOf(o);
	}

	@Override
	public int lastIndexOf(Object o) {
		return backingList.lastIndexOf(o);
	}

	@Override
	public boolean contains(Object o) {
		return backingList.contains(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return backingList.containsAll(c);
	}

	@Override
	public void clear() {
		if (elementObserver != null) {
			final int sz = size();
			for (int i = 0; i < sz; ++i) {
				elementObserver.detachListener(get(i));
			}
		}
		if (hasListeners()) {
			beginChange();
			nextRemove(0, this);
		}
		backingList.clear();
		++modCount;
		if (hasListeners()) {
			endChange();
		}
	}

	@Override
	public void remove(int fromIndex, int toIndex) {
		beginChange();
		for (int i = fromIndex; i < toIndex; ++i) {
			remove(fromIndex);
		}
		endChange();
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		beginChange();
		BitSet bs = new BitSet(c.size());
		for (int i = 0; i < size(); ++i) {
			if (c.contains(get(i))) {
				bs.set(i);
			}
		}
		if (!bs.isEmpty()) {
			int cur = size();
			while ((cur = bs.previousSetBit(cur - 1)) >= 0) {
				remove(cur);
			}
		}
		endChange();
		return !bs.isEmpty();
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		beginChange();
		BitSet bs = new BitSet(c.size());
		for (int i = 0; i < size(); ++i) {
			if (!c.contains(get(i))) {
				bs.set(i);
			}
		}
		if (!bs.isEmpty()) {
			int cur = size();
			while ((cur = bs.previousSetBit(cur - 1)) >= 0) {
				remove(cur);
			}
		}
		endChange();
		return !bs.isEmpty();
	}

	@Override
	public void onChanged(Change<? extends E> change) {
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

	public static class SimplePermutationChange<E> extends NonIterableChange<E> {
		private final int[] permutation;

		public SimplePermutationChange(int from, int to, int[] permutation, ObservableList<E> list) {
			super(from, to, list);
			this.permutation = permutation;
		}

		@Override
		public List<E> getRemoved() {
			checkState();
			return Collections.<E> emptyList();
		}

		@Override
		protected int[] getPermutation() {
			checkState();
			return permutation;
		}
	}

	public static class BindingHelperObserver<E> implements ListChangeListener<E> {
		private final WeakReference<ObservableListWrapper<E>> ref;

		public BindingHelperObserver(ObservableListWrapper<E> transformationList) {
			if (transformationList == null) {
				throw new NullPointerException("Binding has to be specified.");
			}
			ref = new WeakReference<>(transformationList);
		}

		@Override
		public void onChanged(Change<? extends E> change) {
			final ObservableListWrapper<E> binding = ref.get();
			if (binding == null) {
				change.getList().removeListener(this);
			} else {
				binding.onChanged(change);
			}

		}

	}

	public static class ElementObserver<E> {
		private static class ElementsMapElement {
			InvalidationListener listener;
			int counter;

			public ElementsMapElement(InvalidationListener listener) {
				this.listener = listener;
				this.counter = 1;
			}

			public void increment() {
				counter++;
			}

			public int decrement() {
				return --counter;
			}

			private InvalidationListener getListener() {
				return listener;
			}
		}

		private Callback<E, Observable[]> extractor;
		private final Callback<E, InvalidationListener> listenerGenerator;
		private IdentityHashMap<E, ElementObserver.ElementsMapElement> elementsMap = new IdentityHashMap<>();

		ElementObserver(Callback<E, Observable[]> extractor, Callback<E, InvalidationListener> listenerGenerator) {
			this.extractor = extractor;
			this.listenerGenerator = listenerGenerator;
		}

		void attachListener(final E e) {
			if (elementsMap != null && e != null) {
				if (elementsMap.containsKey(e)) {
					elementsMap.get(e).increment();
				} else {
					InvalidationListener listener = listenerGenerator.call(e);
					for (Observable o : extractor.call(e)) {
						o.addListener(listener);
					}
					elementsMap.put(e, new ElementObserver.ElementsMapElement(listener));
				}
			}
		}

		void detachListener(E e) {
			if (elementsMap != null && e != null) {
				ElementObserver.ElementsMapElement el = elementsMap.get(e);
				for (Observable o : extractor.call(e)) {
					o.removeListener(el.getListener());
				}
				if (el.decrement() == 0) {
					elementsMap.remove(e);
				}
			}
		}
	}
}
