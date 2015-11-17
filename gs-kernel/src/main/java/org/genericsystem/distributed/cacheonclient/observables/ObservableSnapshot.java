package org.genericsystem.distributed.cacheonclient.observables;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javafx.beans.InvalidationListener;
import javafx.beans.binding.SetBinding;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableListBase;
import javafx.collections.ObservableSet;
import javafx.collections.SetChangeListener;
import javafx.collections.WeakSetChangeListener;
import org.genericsystem.common.PseudoConcurrentCollection;
import org.genericsystem.common.Vertex;
import com.sun.javafx.collections.SetAdapterChange;
import com.sun.javafx.collections.SetListenerHelper;

public interface ObservableSnapshot<T> extends ObservableSet<T> {

	T get(int index);

	ObservableSnapshot<T> filtered(Predicate<T> predicate);

	ObservableSnapshot<T> concat(ObservableSnapshot<T> toConcatenate);

	ObservableList<T> toObservableList();

	@SuppressWarnings({ "restriction" })
	public static abstract class AbstractObservableSnapshot<E> extends AbstractSet<E> implements ObservableSnapshot<E> {

		@Override
		public ObservableSnapshot<E> filtered(Predicate<E> predicate) {
			return new FilterObservableSnapshotImpl<>(this, predicate);
		}

		@Override
		public ObservableSnapshot<E> concat(ObservableSnapshot<E> toConcatenate) {
			return new ConcatObservableSnapshotImpl<>(this, toConcatenate);
		}

		@Override
		public ObservableList<E> toObservableList() {
			return new ObservableListSnapshot<>(this);
		}

		private SetListenerHelper<E> listenerHelper;

		protected void callObservers(SetChangeListener.Change<E> change) {
			SetListenerHelper.fireValueChangedEvent(listenerHelper, change);
		}

		@Override
		public void addListener(InvalidationListener listener) {
			listenerHelper = SetListenerHelper.addListener(listenerHelper, listener);
		}

		@Override
		public void removeListener(InvalidationListener listener) {
			listenerHelper = SetListenerHelper.removeListener(listenerHelper, listener);
		}

		@Override
		public void addListener(SetChangeListener<? super E> observer) {
			listenerHelper = SetListenerHelper.addListener(listenerHelper, observer);
		}

		@Override
		public void removeListener(SetChangeListener<? super E> observer) {
			listenerHelper = SetListenerHelper.removeListener(listenerHelper, observer);
		}

		class SimpleAddChange extends SetChangeListener.Change<E> {

			private final E added;

			public SimpleAddChange(E added) {
				super(AbstractObservableSnapshot.this);
				this.added = added;
			}

			@Override
			public boolean wasAdded() {
				return true;
			}

			@Override
			public boolean wasRemoved() {
				return false;
			}

			@Override
			public E getElementAdded() {
				return added;
			}

			@Override
			public E getElementRemoved() {
				return null;
			}

			@Override
			public String toString() {
				return "added " + added;
			}

		}

		class SimpleRemoveChange extends SetChangeListener.Change<E> {

			private final E removed;

			public SimpleRemoveChange(E removed) {
				super(AbstractObservableSnapshot.this);
				this.removed = removed;
			}

			@Override
			public boolean wasAdded() {
				return false;
			}

			@Override
			public boolean wasRemoved() {
				return true;
			}

			@Override
			public E getElementAdded() {
				return null;
			}

			@Override
			public E getElementRemoved() {
				return removed;
			}

			@Override
			public String toString() {
				return "removed " + removed;
			}

		}

	}

	public static class ObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

		private final ObservableSnapshot<E> backingSet;

		public ObservableSnapshotImpl(ObservableSnapshot<E> set) {
			this.backingSet = set;
			this.backingSet.addListener(new WeakSetChangeListener<E>(c -> {
				callObservers(new SetAdapterChange<E>(ObservableSnapshotImpl.this, c));
			}));
		}

		@Override
		public int size() {
			return backingSet.size();
		}

		@Override
		public Iterator<E> iterator() {
			return backingSet.iterator();
		}

		@Override
		public E get(int index) {
			return backingSet.get(index);
		}

	}

	public static class ConcatObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

		private final ObservableSnapshot<E> backingSet;
		private final ObservableSnapshot<E> backingSet2;

		public ConcatObservableSnapshotImpl(ObservableSnapshot<E> backingSet, ObservableSnapshot<E> backingSet2) {
			this.backingSet = backingSet;
			this.backingSet2 = backingSet2;
			this.backingSet.addListener(new WeakSetChangeListener<E>(c -> {
				callObservers(new SetAdapterChange<E>(ConcatObservableSnapshotImpl.this, c));
			}));
			this.backingSet2.addListener(new WeakSetChangeListener<E>(c -> {
				callObservers(new SetAdapterChange<E>(ConcatObservableSnapshotImpl.this, c));
			}));
		}

		@Override
		public int size() {
			return backingSet.size() + backingSet2.size();
		}

		@Override
		public Iterator<E> iterator() {
			return Stream.concat(backingSet.stream(), backingSet2.stream()).iterator();
		}

		@Override
		public E get(int index) {
			return index < backingSet.size() ? backingSet.get(index) : backingSet2.get(index - backingSet.size());
		}
	}

	public static class FilterObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

		private final ObservableSnapshot<E> backingSet;
		private final Predicate<E> predicate;
		private int filteredSize;

		public FilterObservableSnapshotImpl(ObservableSnapshot<E> set, Predicate<E> predicate) {
			this.backingSet = set;
			this.predicate = predicate;
			this.filteredSize = Long.valueOf(backingSet.stream().filter(predicate).count()).intValue();
			this.backingSet.addListener(new WeakSetChangeListener<E>(c -> {
				if (c.wasAdded() && predicate.test(c.getElementAdded())) {
					filteredSize++;
					callObservers(new SetAdapterChange<E>(FilterObservableSnapshotImpl.this, c));
				} else if (c.wasRemoved() && predicate.test(c.getElementRemoved())) {
					filteredSize--;
					callObservers(new SetAdapterChange<E>(FilterObservableSnapshotImpl.this, c));
				}
			}));
		}

		@Override
		public int size() {
			assert filteredSize == Long.valueOf(backingSet.stream().filter(predicate).count()).intValue();
			return filteredSize;
		}

		@Override
		public Iterator<E> iterator() {
			return backingSet.stream().filter(predicate).iterator();
		}

		@Override
		public E get(int index) {
			// TODO KK
			Iterator<E> iterator = iterator();
			int i = 0;
			while (iterator.hasNext()) {
				if (index == i)
					return iterator.next();
				iterator.next();
				i++;
			}
			return null;
		}

	}

	public static class ObservableFilterObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

		private final ObservableSnapshot<E> backingSet;
		private final ObservableValue<Predicate<E>> predicate;
		private int filteredSize;

		public ObservableFilterObservableSnapshotImpl(ObservableSnapshot<E> set, ObservableValue<Predicate<E>> predicate) {
			this.backingSet = set;
			this.predicate = predicate;
			predicate.addListener((ChangeListener<Predicate<E>>) (o, oldPredicate, newPredicate) -> {
				filteredSize = Long.valueOf(backingSet.stream().filter(newPredicate).count()).intValue();

				backingSet.stream().forEach(g -> {
					boolean newSelected = newPredicate.test(g);
					if (oldPredicate.test(g) != newSelected)
						if (newSelected)
							callObservers(new SimpleAddChange(g));
						else
							callObservers(new SimpleRemoveChange(g));
				});

			});
			this.filteredSize = Long.valueOf(backingSet.stream().filter(predicate.getValue()).count()).intValue();
			this.backingSet.addListener(new WeakSetChangeListener<E>(c -> {
				if (c.wasAdded() && predicate.getValue().test(c.getElementAdded())) {
					filteredSize++;
					callObservers(new SetAdapterChange<E>(ObservableFilterObservableSnapshotImpl.this, c));
				} else if (c.wasRemoved() && predicate.getValue().test(c.getElementRemoved())) {
					filteredSize--;
					callObservers(new SetAdapterChange<E>(ObservableFilterObservableSnapshotImpl.this, c));
				}
			}));
		}

		@Override
		public int size() {
			assert filteredSize == Long.valueOf(backingSet.stream().filter(predicate.getValue()).count()).intValue();
			return filteredSize;
		}

		@Override
		public Iterator<E> iterator() {
			return backingSet.stream().filter(predicate.getValue()).iterator();
		}

		@Override
		public E get(int index) {
			// TODO KK
			Iterator<E> iterator = iterator();
			int i = 0;
			while (iterator.hasNext()) {
				if (index == i)
					return iterator.next();
				iterator.next();
				i++;
			}
			return null;
		}

	}

	public static class ObservableListSnapshot<E> extends ObservableListBase<E> {
		private final ObservableSnapshot<E> snapshot;

		public ObservableListSnapshot(ObservableSnapshot<E> snapshot) {
			this.snapshot = snapshot;
		}

		@Override
		public E get(int index) {
			return snapshot.get(index);
		}

		@Override
		public int size() {
			return snapshot.size();
		}
	}

	public static class CompletableObservableSnapshot<E> extends AbstractObservableSnapshot<E> {

		private List<E> list = new ArrayList<>();

		public CompletableObservableSnapshot(CompletableFuture<Vertex[]> promise, Function<Vertex, E> extractor) {
			promise.thenAccept(elements -> {
				list = Arrays.stream(elements).map(vertex -> extractor.apply(vertex)).collect(Collectors.toList());
				list.forEach(element -> callObservers(new SimpleAddChange(element)));
			});
		}

		@Override
		public E get(int index) {
			return list.get(index);
		}

		@Override
		public Iterator<E> iterator() {
			return list.iterator();
		}

		@Override
		public int size() {
			return list.size();
		}
	}

	public static class ListWrapperContainerObservableSnapshot<E> extends AbstractObservableSnapshot<E> {

		private List<E> list = new ArrayList<>();

		public ListWrapperContainerObservableSnapshot(List<E> list) {
			this.list = list;
		}

		@Override
		public E get(int index) {
			return list.get(index);
		}

		@Override
		public Iterator<E> iterator() {
			return list.iterator();
		}

		@Override
		public int size() {
			return list.size();
		}
	}

	public static class CompletableObservableSnapshot2<E> extends SetBinding<E> {

		private final CompletableListObservableValue<E> observable;

		public CompletableObservableSnapshot2(CompletableFuture<Vertex[]> promise, Function<Vertex, E> extractor) {
			this.observable = new CompletableListObservableValue<>(extractor);
			bind(observable);
			observable.launch(promise);
		}

		@Override
		protected ObservableSet<E> computeValue() {
			return new ListWrapperContainerObservableSnapshot<>(observable.getValue());
		}
	}

	public static class CompletableListObservableValue<E> extends SimpleObjectProperty<List<E>> implements ObservableValue<List<E>> {

		private final Function<Vertex, E> extractor;

		public CompletableListObservableValue(Function<Vertex, E> extractor) {
			super(new ArrayList<>());
			this.extractor = extractor;
		}

		void launch(CompletableFuture<Vertex[]> promise) {
			promise.thenAccept(elements -> {
				setValue(Arrays.stream(elements).map(extractor::apply).collect(Collectors.toList()));
			});
		}
	}

	// adds removes

	public static class ContainerObservableSnapshot<E> extends AbstractObservableSnapshot<E> {

		PseudoConcurrentCollection<E> container = new PseudoConcurrentCollection<>();

		@Override
		public E get(int index) {
			return container.getByIndex(index);
		}

		@Override
		public Iterator<E> iterator() {
			return container.iterator();
		}

		@Override
		public int size() {
			return container.size();
		}

		@Override
		public boolean add(E element) {
			container.add(element);
			callObservers(new SimpleAddChange(element));
			return true;
		}

		@Override
		public boolean remove(Object element) {
			boolean result = container.remove((E) element);
			if (result)
				callObservers(new SimpleRemoveChange((E) element));
			return result;
		}

	}

}
