package org.genericsystem.api.core;

import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

public enum Filters {

	INSTANCES(vertex -> (x -> vertex.equals(x.getMeta()))), INHERITINGS(vertex -> (x -> x.getSupers().contains(vertex))), COMPOSITES(vertex -> (x -> x.getComponents().contains(vertex)));

	private Function<IGeneric<?>, IndexFilter> getFilter;
	private static ConcurrentHashMap<Filters, ConcurrentHashMap<IGeneric<?>, IndexFilter>> filters = new ConcurrentHashMap<Filters, ConcurrentHashMap<IGeneric<?>, IndexFilter>>() {

		@Override
		public ConcurrentHashMap<IGeneric<?>, IndexFilter> get(Object key) {
			return computeIfAbsent((Filters) key, filter -> new ConcurrentHashMap<IGeneric<?>, IndexFilter>() {

				@Override
				public IndexFilter get(Object key_) {
					return computeIfAbsent((IGeneric<?>) key_, g -> ((Filters) key).getFilter.apply(g));
				}

			});
		}
	};

	Filters(Function<IGeneric<?>, IndexFilter> getFilter) {
		this.getFilter = getFilter;
	}

	public <T extends IGeneric<T>> IndexFilter getFilter(T generic) {
		return filters.get(this).get(generic);
	}

	public static interface IndexFilter<T extends IGeneric<T>> {
		boolean test(T generic);
	}

	public static NoFilter<?> NO_FILTER = new NoFilter<>();

	public static class NoFilter<T extends IGeneric<T>> implements IndexFilter<T> {
		@Override
		public boolean test(T generic) {
			return true;
		}
	}
}
