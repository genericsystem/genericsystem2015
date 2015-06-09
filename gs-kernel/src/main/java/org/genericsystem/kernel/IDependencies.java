package org.genericsystem.kernel;

import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;

public interface IDependencies<T> extends Snapshot<T> {

	@Override
	Stream<T> stream();

	@Override
	T get(Object generic);

	void add(T add);

	public boolean remove(T remove);
}
