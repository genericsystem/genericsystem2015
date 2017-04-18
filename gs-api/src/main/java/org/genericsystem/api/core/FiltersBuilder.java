package org.genericsystem.api.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

public class FiltersBuilder {

	public static interface FilterGetter extends Function<Object[], Predicate<IGeneric<?>>> {
	}

	public static final FilterGetter NO_FILTER = params -> (x -> true);
	public static final FilterGetter INSTANCES = params -> (x -> params[0].equals(x.getMeta()));
	public static final FilterGetter INHERITINGS = params -> (x -> x.getSupers().contains(params[0]));
	public static final FilterGetter COMPOSITES = params -> (x -> x.getComponents().contains(params[0]));
	public static final FilterGetter HAS_META = params -> (x -> !x.equals(params[0]) && x.getMeta().equals(params[0]));
	public static final FilterGetter HAS_SUPER = params -> (x -> x.getSupers().contains(params[0]));
	public static final FilterGetter HAS_COMPONENTS = params -> (x -> filter(x.getComponents(), Arrays.asList(params)).test(x));
	public static final FilterGetter COMPOSITE_HAS_COMPONENTS = params -> (x -> componentsFilter(params).test(x));
	public static final FilterGetter HAS_VALUE = params -> (x -> Objects.equals(x.getValue(), params[0]));
	public static final FilterGetter HAS_SUPERS = params -> (x -> params.length == 0 ? x.getSupers().isEmpty() : filter(x.getSupers(), Arrays.asList(params)).test(x));
	public static final FilterGetter HAS_COMPONENT_AT_POS = params -> (holder -> holder.getComponent((int) params[1]) != null && ((IGeneric) params[0]).isSpecializationOf(holder.getComponent((int) params[1])));
	public static final FilterGetter IS_SPECIALIZATION_OF = params -> (holder -> ((IGeneric) holder).isSpecializationOf((IGeneric) params[0]));
	public static final FilterGetter HAS_LEVEL = params -> (holder -> holder.getLevel() == (int) params[0]);
	public static final FilterGetter NOT_CONTAINED_IN_PARAM = params -> (x -> !((List) params[0]).contains(x));
	public static final FilterGetter IS_DIRECT_DEPENDENCY_OF = params -> (x -> ((IGeneric) params[0]).isDirectAncestorOf(x));

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
}
