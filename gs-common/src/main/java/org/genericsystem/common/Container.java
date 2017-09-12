package org.genericsystem.common;

import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.tools.RxJavaHelpers;

import io.reactivex.Observable;
import javafx.collections.FXCollections;
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
	public Observable<Generic> getAddsObservable() {
		return RxJavaHelpers.additionsOf(container).map(entry -> entry.getKey()).replay().refCount();
	}

	@Override
	public Observable<Generic> getRemovesObservable() {
		return RxJavaHelpers.removalsOf(container).map(entry -> entry.getKey()).replay().refCount();
	}
}