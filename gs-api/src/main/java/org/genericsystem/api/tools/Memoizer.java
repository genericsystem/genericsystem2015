package org.genericsystem.api.tools;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;
import java.util.stream.Stream;

import org.genericsystem.api.core.IGeneric;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;

import io.reactivex.Observable;

public class Memoizer {

	public static <T, U> Function<T, U> memoize(Function<T, U> function) {
		ConcurrentMap<T, U> cache = new ConcurrentHashMap<>();
		return t -> {
			if (cache.containsKey(t))
				return cache.get(t);
			U result = function.apply(t);
			cache.put(t, result);
			return result;
		};
	}

	// Memoized functions for use in Snapshot since they can’t be defined there.
	public static Function<Snapshot<?>, Function<IndexFilter, Snapshot<?>>> getIndexFilterM = memoize(snapshot -> indexFilter -> getSnapshotFilterM(snapshot).apply(indexFilter));

	public static <T> Function<IndexFilter, Snapshot<T>> getSnapshotFilterM(Snapshot<T> parent) {
		return memoize(filter -> new Snapshot<T>() {
			private Observable<T> adds = getParent().getAdds().filter(g -> filter.test((IGeneric<?>) g)).share();
			private Observable<T> removals = getParent().getRemovals().filter(g -> filter.test((IGeneric<?>) g)).share();

			@Override
			public Snapshot<T> getParent() {
				return parent;
			}

			@Override
			public IndexFilter getFilter() {
				return filter;
			}

			@Override
			public Comparator<T> getComparator() {
				return parent.getComparator();
			}

			@Override
			public Observable<T> getAdds() {
				return adds;
			}

			@Override
			public Observable<T> getRemovals() {
				return removals;
			}

			@Override
			public Stream<T> unfilteredStream() {
				throw new UnsupportedOperationException("unfilteredStream() should be called only on unfiltered snapshots.");
			}
		});
	}

	public static Function<Snapshot<?>, Function<List<IndexFilter>, Snapshot<?>>> getIndexListFilterM = memoize(snapshot -> filters -> getSnapshotListFilterM(snapshot).apply(filters));

	public static <T> Function<List<IndexFilter>, Snapshot<T>> getSnapshotListFilterM(Snapshot<T> parent) {
		return memoize(filters -> new Snapshot<T>() {
			private Observable<T> adds = parent.getAdds().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g))).share();
			private Observable<T> removals = parent.getRemovals().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g))).share();

			@Override
			public Stream<T> unfilteredStream() {
				return parent.stream().filter(g -> filters.stream().allMatch(filter -> filter.test((IGeneric<?>) g)));
			}

			@Override
			public Comparator<T> getComparator() {
				return parent.getComparator();
			}

			@Override
			public Observable<T> getAdds() {
				return adds;
			}

			@Override
			public Observable<T> getRemovals() {
				return removals;
			}

			@Override
			public T get(Object o) {
				T result = parent.get(o);
				return result != null && filters.stream().allMatch(filter -> filter.test((IGeneric<?>) result)) ? result : null;
			}
		});
	}
}
