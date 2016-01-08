package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Stream;

import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.IVertex;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.tools.TransitiveObservableList;

import com.sun.javafx.collections.ObservableListWrapper;

public interface DefaultDependencies<T extends DefaultVertex<T>> extends IVertex<T> {

	@SuppressWarnings("unchecked")
	@Override
	default boolean isAlive() {
		return getCurrentCache().isAlive((T) this);
	}

	@Override
	default boolean isAncestorOf(T dependency) {
		return equals(dependency) || (!dependency.isMeta() && isAncestorOf(dependency.getMeta())) || dependency.getSupers().stream().anyMatch(this::isAncestorOf) || dependency.getComponents().stream().filter(x -> x != null).anyMatch(this::isAncestorOf);
	}

	@Override
	default DefaultContext<T> getCurrentCache() {
		return (DefaultContext<T>) getRoot().getCurrentCache();
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getInstance(Serializable value, T... components) {
		return getNonAmbiguousResult(getInstances(value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableInstance(Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableInstances(value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getInstance(T... components) {
		return getNonAmbiguousResult(getInstances(components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableInstance(T... components) {
		return getObservableNonAmbiguousResult(getObservableInstances(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getInstance(T override, Serializable value, T... components) {
		return getNonAmbiguousResult(getInstances(override, value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableInstance(T override, Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableInstances(override, value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getInstance(List<T> overrides, Serializable value, T... components) {
		return getNonAmbiguousResult(getInstances(overrides, value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableInstance(List<T> overrides, Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableInstances(overrides, value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getInstances(Serializable value, T... components) {
		return getInstances(components).filter(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInstances(Serializable value, T... components) {
		return getObservableInstances(components).filtered(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getInstances() {
		return getCurrentCache().getInstances((T) this);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInstances() {
		return getCurrentCache().getObservableInstances((T) this);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getInstances(T... components) {
		return getInstances().filter(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInstances(T... components) {
		return getObservableInstances().filtered(componentsFilter(components));
	}

	@Override
	@SuppressWarnings("unchecked")
	default Snapshot<T> getInstances(T override, Serializable value, T... components) {
		return getInstances(Collections.singletonList(override), value, components);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInstances(T override, Serializable value, T... components) {
		return getObservableInstances(Collections.singletonList(override), value, components);
	}

	@Override
	@SuppressWarnings("unchecked")
	default Snapshot<T> getInstances(List<T> overrides, Serializable value, T... components) {
		List<T> supers = getCurrentCache().computeAndCheckOverridesAreReached((T) this, overrides, value, Arrays.asList(components));
		return getInstances(value, components).filter(overridesFilter(supers));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInstances(List<T> overrides, Serializable value, T... components) {
		List<T> supers = getCurrentCache().computeAndCheckOverridesAreReached((T) this, overrides, value, Arrays.asList(components));
		return getObservableInstances(value, components).filtered(overridesFilter(supers));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getSubInstance(Serializable value, T... components) {
		return getNonAmbiguousResult(getSubInstances(value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableSubInstance(Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableSubInstances(value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInstances(Serializable value, T... components) {
		return getSubInstances(components).filter(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableSubInstances(Serializable value, T... components) {
		return getObservableSubInstances(components).filtered(valueFilter(value));
	}

	@Override
	default Snapshot<T> getSubInstances() {
		return () -> getSubInheritings().stream().flatMap(inheriting -> inheriting.getInstances().stream());
	}

	default ObservableList<T> getObservableSubInstances() {
		return new TransitiveObservableList<T>(getObservableSubInheritings()) {
			@SuppressWarnings({ "unchecked", "restriction" })
			@Override
			protected ObservableList<T> computeValue() {
				final Set<T> set = new LinkedHashSet<>();

				set.add((T) DefaultDependencies.this);
				for (List<T> slave : getSlaves())
					set.addAll(slave);
				return FXCollections.unmodifiableObservableList(new ObservableListWrapper<T>(new ArrayList<>(set)));
			}

			@Override
			protected void onMasterInvalidation() {
				unbindAllSlaves();
				for (T generic : master)
					bindSlave(generic.getObservableInstances());
				invalidate();
			}
		};
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getSubInstance(T... components) {
		return getNonAmbiguousResult(getSubInstances(components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableSubInstance(T... components) {
		return getObservableNonAmbiguousResult(getObservableSubInstances(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInstances(T... components) {
		return getSubInstances().filter(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableSubInstances(T... components) {
		return getObservableSubInstances().filtered(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getSubInstance(T override, Serializable value, T... components) {
		return getNonAmbiguousResult(getSubInstances(override, value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableSubInstance(T override, Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableSubInstances(override, value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInstances(T override, Serializable value, T... components) {
		return getSubInstances(Collections.singletonList(override), value, components);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableSubInstances(T override, Serializable value, T... components) {
		return getObservableSubInstances(Collections.singletonList(override), value, components);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getSubInstance(List<T> overrides, Serializable value, T... components) {
		return getNonAmbiguousResult(getSubInstances(overrides, value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableSubInstance(List<T> overrides, Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableSubInstances(overrides, value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInstances(List<T> overrides, Serializable value, T... components) {
		List<T> supers = getCurrentCache().computeAndCheckOverridesAreReached((T) this, overrides, value, Arrays.asList(components));
		return getSubInstances(value, components).filter(overridesFilter(supers));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableSubInstances(List<T> overrides, Serializable value, T... components) {
		List<T> supers = getCurrentCache().computeAndCheckOverridesAreReached((T) this, overrides, value, Arrays.asList(components));
		return getObservableSubInstances(value, components).filtered(overridesFilter(supers));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getInheriting(Serializable value, T... components) {
		return getNonAmbiguousResult(getInheritings(value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableInheriting(Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableInheritings(value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getInheriting(T... components) {
		return getNonAmbiguousResult(getInheritings(components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableInheriting(T... components) {
		return getObservableNonAmbiguousResult(getObservableInheritings(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getInheritings(Serializable value, T... components) {
		return getInheritings(components).filter(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInheritings(Serializable value, T... components) {
		return getObservableInheritings(components).filtered(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getInheritings() {
		return getCurrentCache().getInheritings((T) this);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInheritings() {
		return getCurrentCache().getObservableInheritings((T) this);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getInheritings(T... components) {
		return getInheritings().filter(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableInheritings(T... components) {
		return getObservableInheritings().filtered(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getSubInheriting(Serializable value, T... components) {
		return getNonAmbiguousResult(getSubInheritings(value, components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableSubInheriting(Serializable value, T... components) {
		return getObservableNonAmbiguousResult(getObservableSubInheritings(value, components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInheritings(Serializable value, T... components) {
		return getSubInheritings(components).filter(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableSubInheritings(Serializable value, T... components) {
		return getObservableSubInheritings(components).filtered(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getSubInheriting(T... components) {
		return getNonAmbiguousResult(getSubInheritings(components).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableSubInheriting(T... components) {
		return getObservableNonAmbiguousResult(getObservableSubInheritings(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInheritings(T... components) {
		return getSubInheritings().filter(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableSubInheritings(T... components) {
		return getObservableSubInheritings().filtered(componentsFilter(components));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getSubInheritings() {
		return () -> Stream.concat(Stream.of((T) this), getInheritings().stream().flatMap(inheriting -> inheriting.getSubInheritings().stream())).distinct();
	}

	default ObservableList<T> getObservableSubInheritings() {
		return new TransitiveObservableList<T>(getObservableInheritings()) {

			@SuppressWarnings({ "unchecked", "restriction" })
			@Override
			protected ObservableList<T> computeValue() {
				final Set<T> set = new HashSet<>();

				set.add((T) DefaultDependencies.this);
				for (List<T> slave : getSlaves())
					set.addAll(slave);
				return FXCollections.unmodifiableObservableList(new ObservableListWrapper<>(new ArrayList<>(set)));
			}

			@Override
			protected void onMasterInvalidation() {
				unbindAllSlaves();
				for (T generic : master)
					bindSlave(generic.getObservableSubInheritings());
				invalidate();
			}
		};
	}

	@Override
	default T getComposite(Serializable value) {
		return getNonAmbiguousResult(getComposites(value).stream());
	}

	default ObservableValue<T> getObservableComposite(Serializable value) {
		return getObservableNonAmbiguousResult(getObservableComposites(value));
	}

	@Override
	default Snapshot<T> getComposites(Serializable value) {
		return getComposites().filter(valueFilter(value));
	}

	default ObservableList<T> getObservableComposites(Serializable value) {
		return getObservableComposites().filtered(valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getComposites() {
		return getCurrentCache().getComposites((T) this);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableComposites() {
		return getCurrentCache().getObservableComposites((T) this);
	}

	static <T extends DefaultVertex<T>> Predicate<T> valueFilter(Serializable value) {
		return attribute -> Objects.equals(attribute.getValue(), value);
	}

	static <T extends DefaultVertex<T>> Predicate<T> overridesFilter(List<T> overrides) {
		return x -> overrides.isEmpty() ? x.getSupers().isEmpty() : filter(x.getSupers(), overrides).test(x);
	}

	@SuppressWarnings("unchecked")
	static <T extends DefaultVertex<T>> Predicate<T> componentsFilter(T... components) {
		return x -> filter(x.getComponents(), Arrays.asList(components)).test(x);
	}

	static <T extends DefaultVertex<T>> Predicate<T> filter(List<T> ancestors, List<T> ancestorsReached) {
		return attribute -> {
			List<T> attributeAncestors = new ArrayList<>(ancestors);
			for (T ancestorsReach : ancestorsReached) {
				T matchedComponent = attributeAncestors.stream().filter(attributeAncestor -> attributeAncestor.equals(ancestorsReach)).findFirst().orElse(null);
				if (matchedComponent != null)
					attributeAncestors.remove(matchedComponent);
				else
					return false;
			}
			return true;
		};
	}

	T getNonAmbiguousResult(Stream<T> stream);

	ObservableValue<T> getObservableNonAmbiguousResult(ObservableList<T> observableList);

}
