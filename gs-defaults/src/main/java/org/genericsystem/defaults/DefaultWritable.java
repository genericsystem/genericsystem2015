package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.IVertex;

public interface DefaultWritable<T extends DefaultVertex<T>> extends IVertex<T> {

	@Override
	default T updateValue(Serializable newValue) {
		return update(getSupers(), newValue, coerceToTArray(getComponents().toArray()));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T updateSupers(T... supers) {
		return update(Arrays.asList(supers), getValue(), coerceToTArray(getComponents().toArray()));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T updateComponent(T targetComponent, int pos) {
		List<T> newComponents = new ArrayList<>(getComponents());
		newComponents.set(pos, targetComponent);
		return getCurrentCache().update((T) this, getSupers(), getValue(), newComponents);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T updateComponents(T... newComponents) {
		return update(getSupers(), getValue(), newComponents);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T update(Serializable newValue, T... newComponents) {
		return update(getSupers(), newValue, newComponents);
	}

	@SuppressWarnings("unchecked")
	@Override
	default public T update(T override, Serializable newValue, T... newComponents) {
		return update(Collections.singletonList(override), newValue, newComponents);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setInstance(Serializable value, T... components) {
		return setInstance(Collections.emptyList(), value, components);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setInstance(T override, Serializable value, T... components) {
		return setInstance(Collections.singletonList(override), value, components);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addInstance(Serializable value, T... components) {
		return addInstance(Collections.emptyList(), value, components);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addInstance(T override, Serializable value, T... components) {
		return addInstance(Collections.singletonList(override), value, components);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addAttribute(Serializable value, T... targets) {
		return addAttribute(Collections.emptyList(), value, targets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setAttribute(Serializable value, T... targets) {
		return setAttribute(Collections.emptyList(), value, targets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addRelation(Serializable value, T firstTarget, T... otherTargets) {
		return addRelation(Collections.emptyList(), value, firstTarget, otherTargets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setRelation(Serializable value, T firstTarget, T... otherTargets) {
		return setRelation(Collections.emptyList(), value, firstTarget, otherTargets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addHolder(T attribute, Serializable value, T... targets) {
		return attribute.addInstance(value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setHolder(T attribute, Serializable value, T... targets) {
		return attribute.setInstance(value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addLink(T relation, Serializable value, T firstTarget, T... otherTargets) {
		return relation.addInstance(value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setLink(T relation, Serializable value, T firstTarget, T... otherTargets) {
		return relation.setInstance(value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addAttribute(T override, Serializable value, T... targets) {
		return addAttribute(Collections.singletonList(override), value, targets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setAttribute(T override, Serializable value, T... targets) {
		return setAttribute(Collections.singletonList(override), value, targets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addRelation(T override, Serializable value, T firstTarget, T... otherTargets) {
		return addRelation(Collections.singletonList(override), value, firstTarget, otherTargets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setRelation(T override, Serializable value, T firstTarget, T... otherTargets) {
		return setRelation(Collections.singletonList(override), value, firstTarget, otherTargets);
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addHolder(T attribute, T override, Serializable value, T... targets) {
		return attribute.addInstance(override, value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setHolder(T attribute, T override, Serializable value, T... targets) {
		return attribute.setInstance(override, value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addLink(T relation, T override, Serializable value, T firstTarget, T... otherTargets) {
		return relation.addInstance(override, value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setLink(T relation, T override, Serializable value, T firstTarget, T... otherTargets) {
		return relation.setInstance(override, value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addAttribute(List<T> overrides, Serializable value, T... targets) {
		return getRoot().addInstance(overrides, value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setAttribute(List<T> overrides, Serializable value, T... targets) {
		return getRoot().setInstance(overrides, value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addRelation(List<T> overrides, Serializable value, T firstTarget, T... otherTargets) {
		return getRoot().addInstance(overrides, value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setRelation(List<T> overrides, Serializable value, T firstTarget, T... otherTargets) {
		return getRoot().setInstance(overrides, value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addHolder(T attribute, List<T> overrides, Serializable value, T... targets) {
		return attribute.addInstance(overrides, value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setHolder(T attribute, List<T> overrides, Serializable value, T... targets) {
		return attribute.setInstance(overrides, value, addThisToTargets(targets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T addLink(T relation, List<T> overrides, Serializable value, T firstTarget, T... otherTargets) {
		return relation.addInstance(overrides, value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	@SuppressWarnings("unchecked")
	default T setLink(T relation, List<T> overrides, Serializable value, T firstTarget, T... otherTargets) {
		return relation.setInstance(overrides, value, addThisToTargets(firstTarget, otherTargets));
	}

	@Override
	default DefaultContext<T> getCurrentCache() {
		return getRoot().getCurrentCache();
	}

	@Override
	public DefaultRoot<T> getRoot();

	@SuppressWarnings("unchecked")
	@Override
	default T[] addThisToTargets(T... targets) {
		T[] composites = getRoot().newTArray(targets.length + 1);
		composites[0] = (T) this;
		System.arraycopy(targets, 0, composites, 1, targets.length);
		return composites;
	}

	@SuppressWarnings("unchecked")
	@Override
	default T[] addThisToTargets(T firstTarget, T... otherTargets) {
		return addThisToTargets(firstTarget.addThisToTargets(otherTargets));
	}

	@SuppressWarnings("unchecked")
	default void forceRemove() {
		getCurrentCache().forceRemove((T) this);
	}

	@SuppressWarnings("unchecked")
	default void conserveRemove() {
		getCurrentCache().conserveRemove((T) this);
	}

	@Override
	@SuppressWarnings("unchecked")
	default void remove() {
		getCurrentCache().remove((T) this);
	}

}
