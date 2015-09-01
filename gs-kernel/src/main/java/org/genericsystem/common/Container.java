package org.genericsystem.common;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;
import org.genericsystem.api.core.IteratorSnapshot;
import org.genericsystem.kernel.Generic;

public class Container implements IteratorSnapshot<Generic> {
	final Map<Generic, Generic> container = new LinkedHashMap<>();

	public Container(Stream<Generic> stream) {
		stream.forEach(add -> container.put(add, add));
	}

	@Override
	public Iterator<Generic> iterator() {
		return container.keySet().iterator();
	}

	@Override
	public Generic get(Object key) {
		return container.get(key);
	}

}