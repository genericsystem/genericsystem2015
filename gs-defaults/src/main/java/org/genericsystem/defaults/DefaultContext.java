package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IContext;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.api.core.exceptions.UnreachableOverridesException;
import org.genericsystem.api.tools.Memoizer;
import org.genericsystem.defaults.tools.SupersComputer;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public interface DefaultContext<T extends DefaultGeneric<T>> extends IContext<T> {

	DefaultRoot<T> getRoot();

	default boolean isAlive(T vertex) {
		assert vertex != null;
		class AliveFinder {
			T find(T vertex) {
				if (vertex.isRoot())
					return vertex;
				if (vertex.isMeta()) {
					T aliveSuper = new AliveFinder().find(vertex.getSupers().get(0));
					return aliveSuper != null ? getInheritings(aliveSuper).get(vertex) : null;
				}
				T aliveMeta = new AliveFinder().find(vertex.getMeta());
				return aliveMeta != null ? getInstances(aliveMeta).get(vertex) : null;
			}
		}
		return /* vertex != null && */vertex.equals(new AliveFinder().find(vertex));
	}

	default Snapshot<T> getInstances(T vertex) {
		return getDependencies(vertex).filter(new IndexFilter(FiltersBuilder.INSTANCES, vertex));
	}

	default Snapshot<T> getInheritings(T vertex) {
		return getDependencies(vertex).filter(new IndexFilter(FiltersBuilder.INHERITINGS, vertex));
	}

	default Snapshot<T> getComposites(T vertex) {
		return getDependencies(vertex).filter(new IndexFilter(FiltersBuilder.COMPOSITES, vertex));
	}

	default void discardWithException(Throwable exception) throws RollbackException {
		throw new RollbackException(exception);
	}

	default NavigableSet<T> computeDependencies(T node) {
		class OrderedDependencies extends TreeSet<T> {
			private static final long serialVersionUID = -441180182522681264L;

			OrderedDependencies visit(T node) {
				if (add(node))
					getDependencies(node).stream().forEach(this::visit);
				return this;
			}
		}
		return new OrderedDependencies().visit(node);
	}

	default NavigableSet<T> computePotentialDependencies(T meta, List<T> supers, Serializable value, List<T> components) {
		// Add back the class PotentialDependenciesComputer if the performance gain from directly adding the dependencies
		// of a node that matches isDependencyOf(meta, supers, value, components) is necessary.
		// Use an array because Java needs a final variable in the lambdas…
		@SuppressWarnings("unchecked")
		Function<T, Boolean>[] memoizedIsDependencyA = new Function[1];
		memoizedIsDependencyA[0] = Memoizer.memoize(node -> node.inheritsFrom(meta, supers, value, components)
				|| node.getComponents().stream().anyMatch(component -> memoizedIsDependencyA[0].apply(component))
				|| !node.isMeta() && memoizedIsDependencyA[0].apply(node.getMeta())
				|| !components.equals(node.getComponents()) && node.componentsDepends(node.getComponents(), components) && supers.stream().anyMatch(override -> override.inheritsFrom(node.getMeta())));
		Function<T, Boolean> memoizedIsDependency = memoizedIsDependencyA[0];

		if (components.isEmpty())
			return meta.isMeta() ? new TreeSet<>(computeDependencies(meta).stream().filter(node -> memoizedIsDependency.apply(node)).collect(Collectors.toSet())) : new TreeSet<>();
			else
				return new TreeSet<>(computeDependencies(components.get(0)).stream().filter(node -> memoizedIsDependency.apply(node)).collect(Collectors.toSet()));
	}

	default NavigableSet<T> computeRemoveDependencies(T node) {
		class OrderedRemoveDependencies extends TreeSet<T> {
			private static final long serialVersionUID = -5970021419012502402L;

			OrderedRemoveDependencies visit(T node) {
				if (!contains(node)) {
					if (!getInheritings(node).isEmpty())
						discardWithException(new ReferentialIntegrityConstraintViolationException("Ancestor : " + node + " has a inheriting dependencies : " + getInheritings(node).info()));
					getInheritings(node).forEach(this::visit);

					if (!getInstances(node).isEmpty())
						discardWithException(new ReferentialIntegrityConstraintViolationException("Ancestor : " + node + " has a instance dependencies : " + getInstances(node).info()));
					getInstances(node).forEach(this::visit);

					for (T composite : getComposites(node)) {
						for (int componentPos = 0; componentPos < composite.getComponents().size(); componentPos++)
							if (composite.getComponents().get(componentPos).equals(node) && !contains(composite) && composite.getMeta().isReferentialIntegrityEnabled(componentPos))
								discardWithException(new ReferentialIntegrityConstraintViolationException(composite + " is Referential Integrity for ancestor " + node + " by composite position : " + componentPos));
						visit(composite);
					}
					add(node);
					for (int axe = 0; axe < node.getComponents().size(); axe++)
						if (node.isCascadeRemoveEnabled(axe))
							visit(node.getComponents().get(axe));
				}
				return this;
			}
		}
		return new OrderedRemoveDependencies().visit(node);
	}

	default List<T> computeAndCheckOverridesAreReached(T adjustedMeta, List<T> overrides, Serializable value, List<T> components) {
		List<T> supers = new ArrayList<>(new SupersComputer<>(adjustedMeta, overrides, value, components));
		if (!ApiStatics.areOverridesReached(supers, overrides))
			discardWithException(new UnreachableOverridesException("Unable to reach overrides : " + overrides + " with computed supers : " + supers));
		return supers;
	}

	Snapshot<T> getDependencies(T vertex);

	default T getMeta(int dim) {
		T adjustedMeta = getRoot().adjustMeta(rootComponents(dim));
		return adjustedMeta != null && adjustedMeta.getComponents().size() == dim ? adjustedMeta : null;
	}

	default T[] rootComponents(int dim) {
		T[] components = getRoot().newTArray(dim);
		Arrays.fill(components, getRoot());
		return components;
	}

}
