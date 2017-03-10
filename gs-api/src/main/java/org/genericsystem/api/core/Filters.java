package org.genericsystem.api.core;

import java.util.function.Function;

public enum Filters {

	INSTANCES(vertex -> (x -> vertex.equals(x.getMeta()))), INHERITINGS(vertex -> (x -> x.getSupers().contains(vertex))), COMPOSITES(vertex -> (x -> x.getComponents().contains(vertex)));

	private Function<IGeneric<?>, IndexFilter> getFilter;

	Filters(Function<IGeneric<?>, IndexFilter> getFilter) {
		this.getFilter = getFilter;
	}

	public <T extends IGeneric<T>> IndexFilter getFilter(T generic) {
		return getFilter.apply(generic);
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
