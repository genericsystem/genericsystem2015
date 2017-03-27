package org.genericsystem.api.core;

import java.util.Objects;

import org.genericsystem.api.core.FiltersBuilder.FilterGetter;

public class IndexFilter {
	private final FilterGetter getFilter;
	private final Object[] params;

	public IndexFilter(FilterGetter getFilter, Object... params) {
		this.getFilter = getFilter;
		this.params = params;
	}

	public final boolean test(IGeneric<?> generic) {
		return getFilter.apply(params).test(generic);
	}

	@Override
	public int hashCode() {
		return getFilter.hashCode() * 31 + (params.length == 0 ? 0 : params[0].hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof IndexFilter))
			return false;
		IndexFilter other = (IndexFilter) obj;
		return getFilter == other.getFilter && Objects.deepEquals(params, other.params);
	}
}