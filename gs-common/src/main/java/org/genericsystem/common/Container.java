package org.genericsystem.common;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;

/**
 * @author Nicolas Feybesse
 *
 */
public class Container implements Snapshot<Generic> {
	final Map<Generic, Generic> container = new LinkedHashMap<>();// TODO is pseudoConcurrrentCollection needed ?

	public Container(Stream<Generic> stream) {
		stream.forEach(add -> container.put(add, add));
	}

	@Override
	public Generic get(Object key) {
		return container.get(key);
	}

	@Override
	public Stream<Generic> unfilteredStream() {
		return container.keySet().stream();
	}
}