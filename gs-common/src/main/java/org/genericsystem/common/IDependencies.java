package org.genericsystem.common;

import java.util.Optional;
import java.util.stream.Stream;

import org.genericsystem.api.core.Filters;
import org.genericsystem.api.core.Snapshot;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public interface IDependencies<T> extends Snapshot<T> {

	@Override
	Stream<T> stream();

	Snapshot<Generic> filter(Filters filter);

	@Override
	public default T get(Object generic) {
		Optional<T> findFirst = stream().filter(generic::equals).findFirst();
		return findFirst.isPresent() ? findFirst.get() : null;
	}

	void add(T add);

	public boolean remove(T remove);
}
