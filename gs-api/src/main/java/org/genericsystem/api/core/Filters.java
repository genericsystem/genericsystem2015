package org.genericsystem.api.core;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import com.sun.javafx.UnmodifiableArrayList;

public class Filters {

	public static interface GetFilter extends Function<UnmodifiableArrayList<IGeneric<?>>, Predicate<IGeneric<?>>> {
	}

	public static final GetFilter NO_FILTER = params -> (x -> true);
	public static final GetFilter INSTANCES = params -> (x -> params.get(0).equals(x.getMeta()));
	public static final GetFilter INHERITINGS = params -> (x -> ((IGeneric<?>) x).getSupers().contains(params.get(0)));
	public static final GetFilter COMPOSITES = params -> (x -> ((IGeneric<?>) x).getComponents().contains(params.get(0)));
	public static final GetFilter BY_META = params -> (x -> !x.equals(params.get(0)) && ((IGeneric<?>) x).getMeta().equals(params.get(0)));
	public static final GetFilter BY_SUPER = params -> (x -> ((IGeneric<?>) x).getSupers().contains(params.get(0)));

	public static class IndexFilter {
		private final GetFilter getFilter;
		private final UnmodifiableArrayList<IGeneric<?>> params;

		public IndexFilter(GetFilter getFilter, IGeneric<?>... params) {
			this.getFilter = getFilter;
			this.params = new UnmodifiableArrayList<>(params, params.length);
		}

		public final boolean test(IGeneric<?> generic) {
			return getFilter.apply(params).test(generic);
		}

		@Override
		public int hashCode() {
			return getFilter.hashCode() * 31 + (params.isEmpty() ? 0 : params.get(0).hashCode());
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof IndexFilter))
				return false;
			IndexFilter other = (IndexFilter) obj;
			return getFilter == other.getFilter && Objects.deepEquals(params, other.params);
		}
	}
}
