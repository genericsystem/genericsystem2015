package org.genericsystem.api.core;

import java.util.Collection;
import java.util.Iterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of IVertex used for all nodes.
 */
public interface IteratorSnapshot<T> extends Snapshot<T> {

	@Override
	public abstract Iterator<T> iterator();

	@Override
	public default Stream<T> stream() {
		return StreamSupport.stream(Spliterators.spliteratorUnknownSize(iterator(), 0), false);
	}

	@Override
	default int size() {
		Iterator<T> iterator = iterator();
		int size = 0;
		while (iterator.hasNext()) {
			iterator.next();
			size++;
		}
		return size;
	}

	@Override
	default boolean isEmpty() {
		return !iterator().hasNext();
	}

	@Override
	default boolean contains(Object o) {
		return o.equals(get(o)); // override necessary
	}

	@Override
	default boolean containsAll(Collection<?> c) {
		return c.stream().allMatch(this::contains);
	}

	@Override
	T get(Object o);

	@Override
	default String info() {
		return stream().collect(Collectors.toList()).toString();
	}

	@Override
	default T first() {
		Iterator<T> iterator = iterator();
		return iterator.hasNext() ? iterator.next() : null;
	}
}
