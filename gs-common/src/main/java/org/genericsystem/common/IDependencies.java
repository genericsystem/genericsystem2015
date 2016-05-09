package org.genericsystem.common;

import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public interface IDependencies<T> extends Snapshot<T> {

	@Override
	Stream<T> stream();

	@Override
	T get(Object generic);

	void add(T add);

	public boolean remove(T remove);
}
