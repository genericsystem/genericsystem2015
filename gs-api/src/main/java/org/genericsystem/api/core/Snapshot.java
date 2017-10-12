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
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.tools.Memoizer;
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
 * This is a functional interface whose functional method is {@link #unfilteredStream}.
 * </p>
 *
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the type of element contained by the <code>Snapshot</code>.
 */
@FunctionalInterface
public interface Snapshot<T> extends Iterable<T> {

	/**
	 * Returns an empty Snapshot.
	 * 
	 * @param <T> Type of the elements of the Snapshot.
	 * @return an empty Snapshot.
	 */
	public static<T> Snapshot<T> empty() {
		return new Snapshot<T>() {
			@Override public Stream<T> unfilteredStream() {
				return Stream.empty();
			}
		};
	}

	/**
	 * Returns a Snapshot containing only an element.
	 * 
	 * @param element The returned Snapshot’s element.
	 * @param <T> Type of the elements of the Snapshot.
	 * @return		  A Snapshot with an element.
	 */
	public static<T> Snapshot<T> singleton(T element) {
		return new Snapshot<T>() {
			@Override
			public Stream<T> unfilteredStream() {
				return Stream.of(element);
			}
		};
	}

	/**
	 * Returns a Snapshot containing the elements of the given Collection.
	 * 
	 * @param elements The returned Snapshot’s elements.
	 * @param <T> Type of the elements of the Snapshot.
	 * @return		   A Snapshot with the given elements.
	 */
	public static<T> Snapshot<T> fromCollection(Collection<T> elements) {
		return new Snapshot<T>() {
			@Override
			public Stream<T> unfilteredStream() {
				return elements.stream();
			}
		};
	}

	/**
	 * Returns this Snapshot’s parent. The return value must be non-null iff
	 * {@link #getFilter()}’s return value is not null.
	 * 
	 * @return This Snapshot’s parent.
	 */
	default Snapshot<T> getParent() {
		return null;
	}

	/**
	 * Returns the {@link IndexFilter} to apply to the parent to build this Snapshot.
	 * The return value must be non-null iff {@link #getParent()}’s return value is not null.
	 * 
	 * @return This Snapshot’s filter.
	 */
	default IndexFilter getFilter() {
		return null;
	}

	/**
	 * Returns an Iterator over the elements of this Snapshot.
	 * 
	 * @return	An iterator for this Snapshot.
	 */
	@Override
	default Iterator<T> iterator() {
		return stream().iterator();
	}

	/**
	 * Returns a <code>Stream</code> of this <code>Snapshot</code>.
	 * This method builds this Snapshot’s stream(), taking into account 
	 * the values of {@link #getFilter()} and {@link #getParent()} if any.
	 * It must not be overridden. To define the Snapshot contents, override 
	 * the methods {@link #unfilteredStream()}, {@link #getAdds()} 
	 * and {@link #getRemovals()}.
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
	 * Defines a Stream of this Snapshot.
	 * If this Snapshot’s {@link #getFilter()}’s method returns {@code null}, the return value of
	 * {@link #stream()} is the same as the return value of unfilteredStream().
	 * This method must be overridden when defining a new Snapshot that can then be filtered
	 * with {@link #filter(IndexFilter)} However, to obtain a Stream of any Snapshot,
	 * {@link #stream()} should be used, never {@link #unfilteredStream()}.
	 *
	 * @return a Stream of this Snapshot.
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

	/**
	 * Returns the Snapshot’s element with the given index.
	 * Returns the element at the given index in the iterator returned by
	 * {@link #iterator()}. If there is no such element, returns {@code null}.
	 * 
	 * @param index The index of the wanted element.
	 * @return		The element at the given index, or {@code null} if there is no
	 * 				element at that index.
	 */
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

	/**
	 * Returns an {@link Observable} emitting the items added to the Snapshot after it was created.
	 * This must be overridden if these updates are necessary, e.g. to update a view of the Snapshot’s contents.
	 * 
	 * @return	An {@link Observable} emitting the elements added to the Snapshot after its creation.
	 * 		 	An empty Observable by default.
	 */
	default Observable<T> getAdds() {
		return Observable.empty();
	}

	/**
	 * Returns an {@link Observable} emitting the items deleted from the Snapshot after it was created.
	 * This must be overridden if these updates are necessary, e.g. to update a view of the Snapshot’s contents.
	 * 
	 * @return	An {@link Observable} emitting the elements removed from the Snapshot since its creation.
	 * 		 	An empty Observable by default.
	 */
	default Observable<T> getRemovals() {
		return Observable.empty();
	}

	/**
	 * Returns the {@link Comparator} for this Snapshot. This is used only to determine the indices of the elements
	 * in {@link #getIndexedElements()}, and to order the emitted sets in {@link #setOnChanged()}.
	 * The Comparator provided must be consistent with equals (see {@link Comparator}).
	 * 
	 * @return The Comparator for this Snapshot. {@code null} by default.
	 */
	default Comparator<T> getComparator() {
		return null;
	}

	/**
	 * Returns an {@link Observable} emitting each element of the Snapshot with its index.
	 * Returns an Observable that emits {@link IndexedElement}s corresponding to:
	 * 	<ul>
	 * 		<li>
	 * 			The items initially contained in this Snapshot, with an index
	 * 			corresponding to their position in a List representing the Snapshot.
	 *		</li>
	 * 		<li>
	 * 			The items added to this Snapshot as they are emitted by the Observable obtained
	 *  		with {@link #getAdds()}, with an index corresponding to their position 
	 *  		in a List representing the Snapshot.
	 * 		</li>
	 * 		<li>
	 * 			The items removed from this Snapshot as they are emitted by the Observable obtained
	 * 			with {@link #getRemovals()}, with an index of -1.
	 *		</li>
	 *	</ul>
	 * 
	 * <p>
	 * <b>Warning:</b> Although this method returns indices as though the Snapshot was represented by a List, it
	 * uses a Set internally to forbid duplicates.
	 * </p>
	 * 
	 * <p>
	 * If {@link #getComparator()} returns a non-null value, the provided {@link Comparator} is used to order
	 * the elements of the Snapshot.
	 * </p>
	 * 
	 * @return An {@link Observable} emitting the indexed elements of this Snapshot.
	 */
	default Observable<IndexedElement<T>> getIndexedElements() {
		Set<T> set = getComparator() != null ? new TreeSet<>(getComparator()) : new HashSet<T>();
		return Observable.merge(Observable.concat(Observable.fromIterable(toList()), getAdds()).map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.ADD)),
				getRemovals().map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.REMOVE)))
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

	/**
	 * Returns an {@link Observable} emitting a {@link Set} representing the Snapshot after each change.
	 * 
	 * If {@link #getComparator()} provides a {@link Comparator} to use for this Snapshot, the emitted set
	 * is ordered with the given comparator.
	 * 
	 * @return An {@link Observable} emitting a {@link Set} representing the Snapshot after each change.
	 */
	default Observable<Set<T>> setOnChanged() {
		Set<T> set = getComparator() != null ? new TreeSet<>(getComparator()) : new HashSet<T>();
		set.addAll(toList());
		return Observable.merge(getAdds().map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.ADD)),
				getRemovals().map(g -> new TaggedElement<T, ChangeType>(g, ChangeType.REMOVE)))
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

	/**
	 * Returns an {@link Observable} emitting a {@link List} representing the Snapshot after each change.
	 * 
	 * The emitted lists contain no duplicates.
	 * 
	 * If {@link #getComparator()} provides a {@link Comparator} to use for this Snapshot, the emitted list
	 * is ordered with the given comparator.
	 * 
	 * @return An {@link Observable} emitting a {@link List} representing the Snapshot after each change.
	 */
	default Observable<List<T>> listOnChanged() {
		return setOnChanged().map(set -> Collections.unmodifiableList(new ArrayList<>(set)));
	}

	/**
	 * Returns an {@link Observable} emitting {@link Optional}s with the first element of the Snapshot.
	 * 
	 * Emits an Optional containing the first element of the Snapshot when that element changes.
	 * Emits an empty Optional if the Snapshot is empty.
	 * 
	 * @return
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	default Observable<Optional<T>> firstOnChanged() {
		return (Observable) listOnChanged().map(list -> list.isEmpty() ? Optional.empty() : Optional.of(list.get(0))).distinctUntilChanged();
	}

	/**
	 * Returns a new Snapshot sorted with the given {@link Comparator}.
	 * 
	 * @param comparator The {@link Comparator} to use for the new Snapshot.
	 * @return			 A Snapshot sorted with the given Comparator.
	 */
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
			public Observable<T> getAdds() {
				return Snapshot.this.getAdds();
			}

			@Override
			public Observable<T> getRemovals() {
				return Snapshot.this.getRemovals();
			}
		};
	}

	/**
	 * Returns a new Snapshot sorted according to the natural order.
	 * 
	 * If the Snapshot’s elements implement {@link Comparable}, the new Snapshot uses
	 * this implementation to sort the elements. Otherwise, the elements are sorted according 
	 * to the Strings returned by toString(), using the {@link Collator} for the current
	 * default Locale.
	 * 
	 * @return			 A Snapshot sorted with the natural order.
	 */
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

	/**
	 * Returns a new Snapshot containing only the elements matching the given {@link Predicate}.
	 * 
	 * If {@link #getComparator()} defines a {@link Comparator} for the current Snapshot, the sort order 
	 * is kept for the returned Snapshot.
	 * 
	 * @param predicate	The predicate used to filter the Snapshot.
	 * @return			A Snapshot filtered with the given predicate.
	 */
	default Snapshot<T> filter(Predicate<T> predicate) {
		return new Snapshot<T>() {
			private Observable<T> adds = Snapshot.this.getAdds().filter(g -> predicate.test(g)).share();
			private Observable<T> removals = Snapshot.this.getRemovals().filter(g -> predicate.test(g)).share();

			@Override
			public Stream<T> unfilteredStream() {
				return Snapshot.this.stream().filter(predicate);
			}

			@Override
			public Comparator<T> getComparator() {
				return Snapshot.this.getComparator();
			}

			@Override
			public Observable<T> getAdds() {
				return adds;
			}

			@Override
			public Observable<T> getRemovals() {
				return removals;
			}

			@Override
			public T get(Object o) {
				T result = Snapshot.this.get(o);
				return result != null && predicate.test(result) ? result : null;
			}
		};
	}

	/**
	 * A new Snapshot filtered with the given {@link IndexFilter}.
	 * 
	 * The returned Snapshot is a child of the current Snapshot.
	 * If {@link #getComparator()} defines a {@link Comparator} for the current Snapshot, the sort order 
	 * is kept for the returned Snapshot.
	 * 
	 * @param filter	The filter to use to select the elements to keep.
	 * @return			A Snapshot filtered with the given IndexFilter.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	default Snapshot<T> filter(IndexFilter filter) {
		return (Snapshot) Memoizer.getIndexFilterM.apply(this).apply(filter);
	}

	/**
	 * A new Snapshot filtered with the given {@link IndexFilter}s.
	 * 
	 * If {@link #getComparator()} defines a {@link Comparator} for the current Snapshot, the sort order 
	 * is kept for the returned Snapshot.
	 * 
	 * @param filters	The filters to use to select the elements to keep.
	 * @return			A Snapshot filtered with the given IndexFilters.
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	default Snapshot<T> filter(List<IndexFilter> filters) {
		return (Snapshot) Memoizer.getIndexListFilterM.apply(this).apply(filters);
	}

	/**
	 * Applies a transformation to each element of the Snapshot.
	 * 
	 * If {@link #getComparator()} defines a {@link Comparator} for the current Snapshot, the sort order 
	 * is kept for the returned Snapshot.
	 * 
	 * @param mapper	The transformation to apply to the Snapshot’s elements.
	 * @param <U>		Type of the elements of the returned Snapshot.
	 * @return			A Snapshot containing the result of the application of the given function
	 * 					to each Snapshot element.
	 */
	default <U> Snapshot<U> map(Function<T, U> mapper) {
		return new Snapshot<U>() {
			private Observable<U> adds = Snapshot.this.getAdds().map(mapper).share();
			private Observable<U> removals = Snapshot.this.getRemovals().map(mapper).share();

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
			public Observable<U> getAdds() {
				return adds;
			}

			@Override
			public Observable<U> getRemovals() {
				return removals;
			}
		};
	}

	/**
	 * Returns a List representation of the Snapshot.
	 * 
	 * @return A list representation of the Snapshot.
	 */
	default List<T> toList() {
		return stream().collect(Collectors.toList());
	}

	default Set<T> toSet() {
		return stream().collect(Collectors.toSet());
	}

	/**
	 * Returns an ObservableList representing the Snapshot’s state.
	 * 
	 * Deprecated because is does not allow disposal of the subscriptions 
	 * to {@link #getAdds()} and {@link #getRemovals()}.
	 * 
	 * @return an ObservableList representing this Snapshot.
	 */
	@Deprecated
	default ObservableList<T> toObservableList() {
		Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
		ObservableList<T> list = FXCollections.observableArrayList(toList());
		CompositeDisposable disposables = new CompositeDisposable();
		disposables.add(getAdds().subscribe(g -> {
			if (!list.contains(g)) {
				list.add(g);
				logger.debug("Snapshot {}, generic added, {}", System.identityHashCode(this), g);
			}
		}, e -> logger.error("Exception while computing observable list.", e)));
		disposables.add(getRemovals().subscribe(g -> {
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

	/**
	 * Helper class whose instances contain an element of type T and an int index.
	 * Used by {@link Snapshot#getIndexedElements()} to return both an added or removed
	 * element with the corresponding index.
	 * 
	 * @param <T>	Type of the indexed elements.
	 */
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
