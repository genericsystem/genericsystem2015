package org.genericsystem.common;

import java.util.Optional;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class IDependencies<T> extends Snapshot<T> {

	@Override
	public abstract Stream<T> rootStream();

	@Override
	public T get(Object generic) {
		Optional<T> findFirst = stream().filter(generic::equals).findFirst();
		return findFirst.isPresent() ? findFirst.get() : null;
	}

	public abstract void add(T add);

	public abstract boolean remove(T remove);
}
