package org.genericsystem.defaults;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IVertex;

public interface DefaultAncestors<T extends DefaultVertex<T>> extends IVertex<T> {

	@Override
	default boolean isRoot() {
		return this.equals(getRoot());
	}

	@Override
	default int getLevel() {
		return this == getMeta() ? 0 : getMeta().getLevel() + 1;
	}

	@Override
	DefaultRoot<T> getRoot();

	// @SuppressWarnings("unchecked")
	// @Override
	// default DefaultRoot<T> getRoot() {
	// return !isMeta() ? getMeta().getRoot() : getSupers().isEmpty() ? (DefaultRoot<T>) this : getSupers().get(0).getRoot();
	// }

	@Override
	default boolean isMeta() {
		return getLevel() == ApiStatics.META;
	}

	@Override
	default boolean isStructural() {
		return getLevel() == ApiStatics.STRUCTURAL;
	}

	@Override
	default boolean isConcrete() {
		return getLevel() == ApiStatics.CONCRETE;
	}

	default boolean isDirectAncestorOf(T vertex) {
		return !equals(vertex) && (equals(vertex.getMeta()) || vertex.getSupers().contains(this) || vertex.getComponents().contains(this));
	}

	@Override
	default boolean inheritsFrom(T superVertex) {
		if (equals(superVertex))
			return true;
		if (getLevel() != superVertex.getLevel())// Not necessary
			return false;
		for (T superT : getSupers())
			if (superT.inheritsFrom(superVertex))
				return true;
		return false;
	}

	@Override
	default boolean isInstanceOf(T metaVertex) {
		return getMeta().inheritsFrom(metaVertex);
	}

	@Override
	default boolean isSpecializationOf(T supra) {
		return getLevel() == supra.getLevel() ? inheritsFrom(supra) : (getLevel() > supra.getLevel() && getMeta().isSpecializationOf(supra));
	}

	@Override
	default boolean isCompositeOf(T vertex) {
		return getComponents().stream().anyMatch(component -> vertex.isSpecializationOf(component));
	}

	default boolean isCompositeForInstances(T vertex) {
		return getComponents().stream().anyMatch(component -> vertex.inheritsFrom(component));
	}

	@Override
	default T getComponent(int pos) {
		return pos >= 0 && pos < getComponents().size() ? getComponents().get(pos) : null;
	}

	@Override
	default T getBaseComponent() {
		return getComponent(ApiStatics.BASE_POSITION);
	}

	@Override
	default T getTargetComponent() {
		return getComponent(ApiStatics.TARGET_POSITION);
	}

	@Override
	default T getTernaryComponent() {
		return getComponent(ApiStatics.TERNARY_POSITION);
	}
}
