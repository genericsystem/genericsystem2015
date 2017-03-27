package org.genericsystem.api.core;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
public class Snapshot<T> implements Iterable<T> {

	private final Snapshot<T> parent;
	private final IndexFilter filter;

	public Snapshot() {
		parent = null;
		filter = null;
	}

	public Snapshot(Snapshot<T> parent, IndexFilter filter) {
		assert parent != null && filter != null;
		this.parent = parent;
		this.filter = filter;
	}

	@Override
	public Iterator<T> iterator() {
		return stream().iterator();
	}

	/**
	 * Returns a <code>Stream</code> of this <code>Snapshot</code>.
	 *
	 * @return a <code>Stream</code> of this <code>Snapshot</code>.
	 */
	public final Stream<T> stream() {
		List<IndexFilter> filters = new LinkedList<>();
		Snapshot<T> current = this;
		while (current.parent != null) {
			filters.add(0, current.filter);
			current = current.parent;
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
	public Stream<T> unfilteredStream() {
		throw new UnsupportedOperationException("The unfiltered Stream() method must be overridden by extending classes.");
	}

	/**
	 * Returns the number of elements in this snapshot.
	 *
	 * @return the number of elements in this snapshot.
	 */
	public int size() {
		return (int) stream().count();
	}

	/**
	 * Returns <code>true</code> if this snapshot contains no elements.
	 *
	 * @return <code>true</code> if this snapshot contains no elements.
	 */
	public boolean isEmpty() {
		return stream().count() == 0;
	}

	/**
	 * Returns <code>true</code> if this snapshot contains the specified element.
	 *
	 * @param o
	 *            element whose presence in this snapshot is to be tested.
	 * @return <code>true</code> if this snapshot contains the specified element.
	 */
	public boolean contains(Object o) {
		return o.equals(get(o));
	}

	/**
	 * Returns <code>true</code> if this snapshot contains all of the elements in the specified snapshot.
	 *
	 * @param c
	 *            collection to be checked for containment in this snapshot.
	 * @return <code>true</code> if this snapshot contains all of the elements in the specified snapshot.
	 */
	public boolean containsAll(Collection<?> c) {
		return c.stream().allMatch(this::contains);
	}

	/**
	 * Returns the first element in this snapshot equals to the specified object or <code>null</code> if no element in this snapshot is equal to the specified object.
	 *
	 * @param o
	 *            object to be tested for equality.
	 * @return the first element in this snapshot equals to the specified object or <code>null</code> if no element in this snapshot is equal to the specified object.
	 */
	public T get(Object o) {
		return stream().filter(o::equals).findFirst().orElse(null);
	}

	/**
	 * Returns a <code>String</code> representation of all vertices contained in this snapshot.
	 *
	 * @return a <code>String</code> representation of all vertices contained in this snapshot.
	 */
	public String info() {
		return stream().collect(Collectors.toList()).toString();
	}

	/**
	 * Returns the first element of this snapshot or <code>null</code> if this snapshot is empty.
	 *
	 * @return the first element of this snapshot or <code>null</code> if this snapshot is empty.
	 */

	public T first() {

		return (iterator().hasNext() ? iterator().next() : null);

	}

	public T getByIndex(int index) {
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

	public Snapshot<T> filter(Predicate<T> predicate) {
		return new Snapshot<T>() {

			@Override
			public Stream<T> unfilteredStream() {
				return Snapshot.this.stream().filter(predicate);
			}

			@Override
			public T get(Object o) {
				T result = Snapshot.this.get(o);
				return result != null && predicate.test(result) ? result : null;
			}
		};
	}

	public Snapshot<T> filter(IndexFilter filter) {
		return new Snapshot<T>(this, filter);
	}

	public Snapshot<T> filter(List<IndexFilter> filters) {
		assert filter == null;
		return new Snapshot<T>() {

			@Override
			public Stream<T> unfilteredStream() {
				return Snapshot.this.stream().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g)));
			}

			@Override
			public T get(Object o) {
				T result = Snapshot.this.get(o);
				return result != null && filters.stream().allMatch(filter -> filter.test((IGeneric<?>) result)) ? result : null;
			}
		};
	}

	public List<T> toList() {
		return stream().collect(Collectors.toList());
	}
}
