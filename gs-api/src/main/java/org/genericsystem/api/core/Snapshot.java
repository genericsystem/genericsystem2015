package org.genericsystem.api.core;

import java.util.Collection;
import java.util.Iterator;
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
@FunctionalInterface
public interface Snapshot<T> extends Iterable<T> {

	@Override
	default Iterator<T> iterator() {
		return stream().iterator();
	}

	/**
	 * Returns a <code>Stream</code> of this <code>Snapshot</code>.
	 *
	 * @return a <code>Stream</code> of this <code>Snapshot</code>.
	 */
	abstract Stream<T> stream();

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
		return stream().findFirst().orElse(null);
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

	default Snapshot<T> filter(Predicate<T> predicate) {
		return new Snapshot<T>() {

			@Override
			public Stream<T> stream() {
				return Snapshot.this.stream().filter(predicate);
			}

			@Override
			public T get(Object o) {
				T result = Snapshot.this.get(o);
				return result != null && predicate.test(result) ? result : null;
			}
		};
	}
	
	default List<T> toList(){
		return stream().collect(Collectors.toList());
	}
}
