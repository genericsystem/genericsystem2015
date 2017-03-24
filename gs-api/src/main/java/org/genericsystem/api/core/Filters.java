package org.genericsystem.api.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import com.sun.javafx.UnmodifiableArrayList;

public class Filters {

	public static interface GetFilter extends Function<UnmodifiableArrayList<Object>, Predicate<IGeneric<?>>> {
	}

	public static final GetFilter NO_FILTER = params -> (x -> true);
	public static final GetFilter INSTANCES = params -> (x -> params.get(0).equals(x.getMeta()));
	public static final GetFilter INHERITINGS = params -> (x -> x.getSupers().contains(params.get(0)));
	public static final GetFilter COMPOSITES = params -> (x -> x.getComponents().contains(params.get(0)));
	public static final GetFilter HAS_META = params -> (x -> !x.equals(params.get(0)) && x.getMeta().equals(params.get(0)));
	public static final GetFilter HAS_SUPER = params -> (x -> x.getSupers().contains(params.get(0)));
	public static final GetFilter HAS_COMPONENTS = params -> (x -> filter(x.getComponents(), params).test(x));
	public static final GetFilter COMPOSITE_HAS_COMPONENTS = params -> (x -> componentsFilter(params.stream().toArray()).test(x));
	public static final GetFilter HAS_VALUE = params -> (x -> Objects.equals(x.getValue(), params.get(0)));
	public static final GetFilter HAS_SUPERS = params -> (x -> params.isEmpty() ? x.getSupers().isEmpty() : filter(x.getSupers(), params).test(x));

	static Predicate<IGeneric<?>> filter(List<?> ancestors, List<Object> ancestorsReached) {
		return attribute -> {
			List<?> attributeAncestors = new ArrayList<>(ancestors);
			for (Object ancestorsReach : ancestorsReached) {
				Object matchedComponent = attributeAncestors.stream().filter(attributeAncestor -> attributeAncestor.equals(ancestorsReach)).findFirst().orElse(null);
				if (matchedComponent != null)
					attributeAncestors.remove(matchedComponent);
				else
					return false;
			}
			return true;
		};
	}

	@SuppressWarnings("unchecked")
	static <T extends IGeneric<T>> Predicate<IGeneric<?>> componentsFilter(Object... componentsReached) {
		return attribute -> {
			List<T> attributeComps = new ArrayList<>(((IGeneric<T>) attribute).getComponents());
			for (Object componentReach : componentsReached) {
				T matchedComponent = attributeComps.stream().filter(attributeComp -> ((IGeneric<T>) componentsReached[0]).isSpecializationOf(attributeComp) ? true : componentReach.equals(attributeComp)).findFirst().orElse(null);
				if (matchedComponent != null)
					attributeComps.remove(matchedComponent);
				else
					return false;
			}
			return true;
		};
	}

	public static class IndexFilter {
		private final GetFilter getFilter;
		private final UnmodifiableArrayList<Object> params;

		public IndexFilter(GetFilter getFilter, Object... params) {
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
