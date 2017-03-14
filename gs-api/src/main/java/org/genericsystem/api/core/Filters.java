package org.genericsystem.api.core;

import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

import com.sun.javafx.UnmodifiableArrayList;

public enum Filters {

	INSTANCES(vertices -> (x -> vertices.get(0).equals(((IGeneric<?>) x).getMeta()))), INHERITINGS(vertices -> (x -> ((IGeneric<?>) x).getSupers().contains(vertices.get(0)))), COMPOSITES(
			vertices -> (x -> ((IGeneric<?>) x).getComponents().contains(vertices.get(0)))), COMPOSITES_BY_META(
					vertices -> (x -> ((IGeneric<?>) x).getComponents().contains(vertices.get(0)) && !x.equals(vertices.get(1)) && ((IGeneric<?>) x).getMeta().equals(vertices.get(1)))), COMPOSITES_BY_SUPER(
							vertices -> (x -> ((IGeneric<?>) x).getComponents().contains(vertices.get(0)) && ((IGeneric<?>) x).getSupers().contains(vertices.get(1))));

	private Function<UnmodifiableArrayList<IGeneric<?>>, IndexFilter> getFilter;
	private static ConcurrentHashMap<Filters, ConcurrentHashMap<UnmodifiableArrayList<IGeneric<?>>, IndexFilter>> filters = new ConcurrentHashMap<Filters, ConcurrentHashMap<UnmodifiableArrayList<IGeneric<?>>, IndexFilter>>() {

		@Override
		public ConcurrentHashMap<UnmodifiableArrayList<IGeneric<?>>, IndexFilter> get(Object key) {
			return computeIfAbsent((Filters) key, filter -> new ConcurrentHashMap<UnmodifiableArrayList<IGeneric<?>>, IndexFilter>() {

				@Override
				public IndexFilter get(Object key_) {
					return computeIfAbsent((UnmodifiableArrayList<IGeneric<?>>) key_, gs -> ((Filters) key).getFilter.apply(gs));
				}

			});
		}
	};

	Filters(Function<UnmodifiableArrayList<IGeneric<?>>, IndexFilter> getFilter) {
		this.getFilter = getFilter;
	}

	public <T extends IGeneric<T>> IndexFilter getFilter(T... generics) {
		return filters.get(this).get(new UnmodifiableArrayList<>(generics, generics.length));
	}

	public static interface IndexFilter<T> {
		boolean test(T generic);
	}

	public static NoFilter<?> NO_FILTER = new NoFilter<>();

	public static class NoFilter<T> implements IndexFilter<T> {
		@Override
		public boolean test(T generic) {
			return true;
		}
	}
}
