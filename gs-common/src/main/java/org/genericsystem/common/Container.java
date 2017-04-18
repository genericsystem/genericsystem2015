package org.genericsystem.common;

import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;

import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

/**
 * @author Nicolas Feybesse
 *
 */
public class Container implements Snapshot<Generic> {
	final ObservableMap<Generic, Generic> container = FXCollections.observableHashMap();// TODO is pseudoConcurrrentCollection needed?

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

	@Override
	public ObservableList<Generic> toObservableList() {
		ObservableList<Generic> result = FXCollections.observableArrayList();
		container.addListener((MapChangeListener<Generic, Generic>) c -> {
			if (c.wasAdded() && !c.wasRemoved())
				result.add(c.getKey());
			if (c.wasRemoved() && !c.wasAdded())
				result.remove(c.getKey());
		});
		return result;
	}
}