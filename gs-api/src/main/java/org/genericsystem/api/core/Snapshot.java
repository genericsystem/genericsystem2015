package org.genericsystem.api.core;

import java.lang.invoke.MethodHandles;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.Observable;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.functions.Function;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * Represents a <code>Set</code> of results <em>aware</em> of its context.
 * <p>
 * This is a functional interface whose functional method is {@link #get}.
 * </p>
 *
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the type of element contained by the <code>Snapshot</code>.
 */
@FunctionalInterface
public interface Snapshot<T> extends Iterable<T> {

	public static<T> Snapshot<T> empty() {
		return new Snapshot<T>() {
			@Override public Stream<T> unfilteredStream() {
				return Stream.empty();
			}
		};
	}

	public static<T> Snapshot<T> singleton(T element) {
		return new Snapshot<T>() {
			@Override
			public Stream<T> unfilteredStream() {
				return Stream.of(element);
			}
		};
	}

	public static<T> Snapshot<T> fromCollection(Collection<T> elements) {
		return new Snapshot<T>() {
			@Override
			public Stream<T> unfilteredStream() {
				return elements.stream();
			}
		};
	}

	default Snapshot<T> getParent() {
		return null;
	}

	default IndexFilter getFilter() {
		return null;
	}

	@Override
	default Iterator<T> iterator() {
		return stream().iterator();
	}

	/**
	 * Returns a <code>Stream</code> of this <code>Snapshot</code>.
	 *
	 * @return a <code>Stream</code> of this <code>Snapshot</code>.
	 */
	default Stream<T> stream() {
		List<IndexFilter> filters = new LinkedList<>();
		Snapshot<T> current = this;
		while (current.getParent() != null) {
			filters.add(0, current.getFilter());
			current = current.getParent();
		}
		if (!filters.isEmpty())
			return current.filter(filters).unfilteredStream();
		return unfilteredStream();
	}

	/**
	 * Returns a <code>Stream</code> of this <code>Snapshot</code>. Used only if this <code>Snapshot</code> is not filtered.
	 *
	 * @return a <code>Stream</code> of this <code>Snapshot</code>.
	 */
	public Stream<T> unfilteredStream();

	/**
	 * Returns the number of elements in this snapshot.
	 *
	 * @return the number of elements in this snapshot.
	 */
	default int size() {
		return (int) stream().count();
	}

	/**
	 * Returns <code>true</code> if this snapshot contains no elements.
	 *
	 * @return <code>true</code> if this snapshot contains no elements.
	 */
	default boolean isEmpty() {
		return stream().count() == 0;
	}

	/**
	 * Returns <code>true</code> if this snapshot contains the specified element.
	 *
	 * @param o
	 *            element whose presence in this snapshot is to be tested.
	 * @return <code>true</code> if this snapshot contains the specified element.
	 */
	default boolean contains(Object o) {
		return o.equals(get(o));
	}

	/**
	 * Returns <code>true</code> if this snapshot contains all of the elements in the specified snapshot.
	 *
	 * @param c
	 *            collection to be checked for containment in this snapshot.
	 * @return <code>true</code> if this snapshot contains all of the elements in the specified snapshot.
	 */
	default boolean containsAll(Collection<?> c) {
		return c.stream().allMatch(this::contains);
	}

	/**
	 * Returns the first element in this snapshot equals to the specified object or <code>null</code> if no element in this snapshot is equal to the specified object.
	 *
	 * @param o
	 *            object to be tested for equality.
	 * @return the first element in this snapshot equals to the specified object or <code>null</code> if no element in this snapshot is equal to the specified object.
	 */
	default T get(Object o) {
		return stream().filter(o::equals).findFirst().orElse(null);
	}

	/**
	 * Returns a <code>String</code> representation of all vertices contained in this snapshot.
	 *
	 * @return a <code>String</code> representation of all vertices contained in this snapshot.
	 */
	default String info() {
		return stream().collect(Collectors.toList()).toString();
	}

	/**
	 * Returns the first element of this snapshot or <code>null</code> if this snapshot is empty.
	 *
	 * @return the first element of this snapshot or <code>null</code> if this snapshot is empty.
	 */
	default T first() {
		return (iterator().hasNext() ? iterator().next() : null);
	}

	default T getByIndex(int index) {
		Iterator<T> iterator = iterator();
		int i = 0;
		while (iterator.hasNext()) {
			if (index == i)
				return iterator.next();
			iterator.next();
			i++;
		}
		return null;
	}

	default Observable<T> getAddsObservable() {
		return Observable.empty();
	}

	default Observable<T> getRemovesObservable() {
		return Observable.empty();
	}

	default Comparator<T> getComparator() {
		return null;
	}

	// No duplicates
	default Observable<IndexedElement<T>> getIndexedElements() {
		Set<T> set = getComparator() != null ? new TreeSet<>(getComparator()) : new HashSet<T>();
		return Observable.merge(Observable.concat(Observable.fromIterable(toList()), getAddsObservable()).map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.ADD)),
				getRemovesObservable().map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.REMOVE)))
				.scan(new TaggedElement<Set<T>, IndexedElement<T>>(set, null), (acc, change) -> {
					if (change.tag == ChangeType.ADD) {
						if (acc.element.add(change.element)) {
							if (getComparator() != null)
								return new TaggedElement<>(acc.element, new IndexedElement<T>(change.element, Collections.binarySearch(new ArrayList<T>(acc.element), change.element, getComparator())));
							else
								return new TaggedElement<>(acc.element, new IndexedElement<T>(change.element, new ArrayList<T>(acc.element).indexOf(change.element)));
						}
						return new TaggedElement<>(acc.element, null);
					} else {
						acc.element.remove(change.element);
						return new TaggedElement<>(acc.element, new IndexedElement<T>(change.element, -1));
					}
				}).filter(tagElt -> tagElt.tag != null).map(tagElt -> tagElt.tag);
	}

	// No duplicates
	default Observable<Set<T>> setOnChanged() {
		Set<T> set = getComparator() != null ? new TreeSet<>(getComparator()) : new HashSet<T>();
		set.addAll(toList());
		return Observable.merge(getAddsObservable().map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.ADD)),
				getRemovesObservable().map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.REMOVE)))
				.scan(new TaggedElement<Set<T>, Boolean>(set, true), (acc, change) -> {
					if (change.tag == ChangeType.ADD) {
						if (acc.element.add(change.element))
							return new TaggedElement<>(acc.element, true);
						return new TaggedElement<>(acc.element, false);
					} else {
						if (acc.element.remove(change.element))
							return new TaggedElement<>(acc.element, true);
						return new TaggedElement<>(acc.element, false);
					}
				}).filter(tagElt -> tagElt.tag).map(tagElt -> Collections.unmodifiableSet(tagElt.element));
	}

	default Snapshot<T> sort(Comparator<T> comparator) {
		return new Snapshot<T>() {
			@Override
			public Stream<T> unfilteredStream() {
				return Snapshot.this.stream();
			}

			@Override
			public Comparator<T> getComparator() {
				return comparator;
			}

			@Override
			public Observable<T> getAddsObservable() {
				return Snapshot.this.getAddsObservable();
			}

			@Override
			public Observable<T> getRemovesObservable() {
				return Snapshot.this.getRemovesObservable();
			}
		};
	}

	default Snapshot<T> sorted() {
		Comparator<T> naturalOrder = new Comparator<T>() {

			@SuppressWarnings("unchecked")
			@Override
			public int compare(T o1, T o2) {
				if (o1 == null && o2 == null)
					return 0;
				if (o1 == null)
					return -1;
				if (o2 == null)
					return 1;

				if (o1 instanceof Comparable) {
					return ((Comparable<T>) o1).compareTo(o2);
				}

				return Collator.getInstance().compare(o1.toString(), o2.toString());
			}
		};
		return sort(naturalOrder);
	}

	default Snapshot<T> filter(Predicate<T> predicate) {
		return new Snapshot<T>() {

			@Override
			public Stream<T> unfilteredStream() {
				return Snapshot.this.stream().filter(predicate);
			}

			@Override
			public Comparator<T> getComparator() {
				return Snapshot.this.getComparator();
			}

			@Override
			public Observable<T> getAddsObservable() {
				return Snapshot.this.getAddsObservable().filter(g -> predicate.test(g)).replay().refCount();
			}

			@Override
			public Observable<T> getRemovesObservable() {
				return Snapshot.this.getRemovesObservable().filter(g -> predicate.test(g)).replay().refCount();
			}

			@Override
			public T get(Object o) {
				T result = Snapshot.this.get(o);
				return result != null && predicate.test(result) ? result : null;
			}
		};
	}

	default Snapshot<T> filter(IndexFilter filter) {
		return new Snapshot<T>() {
			@Override
			public Snapshot<T> getParent() {
				return Snapshot.this;
			}

			@Override
			public IndexFilter getFilter() {
				return filter;
			}

			@Override
			public Comparator<T> getComparator() {
				return Snapshot.this.getComparator();
			}

			@Override
			public Observable<T> getAddsObservable() {
				return getParent().getAddsObservable().filter(g -> filter.test((IGeneric<?>) g)).replay().refCount();
			}

			@Override
			public Observable<T> getRemovesObservable() {
				return getParent().getRemovesObservable().filter(g -> filter.test((IGeneric<?>) g)).replay().refCount();
			}

			@Override
			public Stream<T> unfilteredStream() {
				throw new UnsupportedOperationException("unfilteredStream() should be called only on unfiltered snapshots.");
			}
		};
	}

	default Snapshot<T> filter(List<IndexFilter> filters) {
		return new Snapshot<T>() {

			@Override
			public Stream<T> unfilteredStream() {
				return Snapshot.this.stream().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g)));
			}

			@Override
			public Comparator<T> getComparator() {
				return Snapshot.this.getComparator();
			}

			@Override
			public Observable<T> getAddsObservable() {
				return Snapshot.this.getAddsObservable().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g))).replay().refCount();
			}

			@Override
			public Observable<T> getRemovesObservable() {
				return Snapshot.this.getRemovesObservable().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g))).replay().refCount();
			}

			@Override
			public T get(Object o) {
				T result = Snapshot.this.get(o);
				return result != null && filters.stream().allMatch(filter -> filter.test((IGeneric<?>) result)) ? result : null;
			}
		};
	}

	/**
	 * Applies a transformation to each element of the snapshot. If the source Snapshot is sorted,
	 * the source order is preserved.
	 * 
	 * @param mapper	The transformation to apply to the Snapshot’s elements.
	 * @return			A Snapshot containing the result of the application of the given function
	 * 					to each Snapshot element.
	 */
	default <U> Snapshot<U> map(Function<T, U> mapper) {
		return new Snapshot<U>() {
			@Override
			public Stream<U> unfilteredStream() {
				return Snapshot.this.stream().map(e -> {
					try {
						return mapper.apply(e);
					} catch (Exception ex) {
						throw new IllegalStateException("Exception while handling Snapshot.", ex);
					}
				});
			}

			// Keep order from before the mapping.
			@Override
			public Observable<IndexedElement<U>> getIndexedElements() {
				return Snapshot.this.getIndexedElements().map(ie -> new IndexedElement<>(mapper.apply(ie.getElement()), ie.getIndex()));
			}

			@Override
			public Observable<U> getAddsObservable() {
				return Snapshot.this.getAddsObservable().map(mapper);
			}

			@Override
			public Observable<U> getRemovesObservable() {
				return Snapshot.this.getRemovesObservable().map(mapper);
			}
		};
	}

	default List<T> toList() {
		return stream().collect(Collectors.toList());
	}

	/**
	 * Returns an ObservableList representing the Snapshot’s state. Deprecated because is does not
	 * allow disposal of the subscriptions to getAddsObservable() and getRemovesObservable().
	 * 
	 * @return an ObservableList representing this Snapshot.
	 */
	@Deprecated
	default ObservableList<T> toObservableList() {
		Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
		ObservableList<T> list = FXCollections.observableArrayList(toList());
		CompositeDisposable disposables = new CompositeDisposable();
		disposables.add(getAddsObservable().subscribe(g -> {
			if (!list.contains(g)) {
				list.add(g);
				logger.debug("Snapshot {}, generic added, {}", System.identityHashCode(this), g);
			}
		}, e -> logger.error("Exception while computing observable list.", e)));
		disposables.add(getRemovesObservable().subscribe(g -> {
			list.remove(g);
			logger.debug("Snapshot {}, generic removed, {}", System.identityHashCode(this), g);
		}, e -> logger.error("Exception while computing observable list.", e)));
		return list;
	}

	public static enum ChangeType {
		ADD, REMOVE;
	}

	public static class TaggedElement<T, U> {
		protected final T element;
		protected final U tag;

		public TaggedElement(T element, U tag) {
			this.element = element;
			this.tag = tag;
		}
	}

	public static class IndexedElement<T> {
		private final int index;
		private final T element;

		public IndexedElement(T element, int index) {
			this.element = element;
			this.index = index;
		}

		public int getIndex() {
			return index;
		}

		public T getElement() {
			return element;
		}

		@Override
		public int hashCode() {
			return element.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof IndexedElement))
				return false;
			IndexedElement<?> other = (IndexedElement<?>) obj;
			return other.index == index && element.equals(other.element);
		}
	}
}
