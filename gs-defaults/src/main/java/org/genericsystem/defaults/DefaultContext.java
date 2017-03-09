package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Filters;
import org.genericsystem.api.core.IContext;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.api.core.exceptions.UnreachableOverridesException;
import org.genericsystem.defaults.tools.SupersComputer;

import javafx.collections.ObservableList;

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
				if (vertex.isRoot()) {
					System.out.println("isAlive, isRoot " + vertex);
					return vertex;
				}
				if (vertex.isMeta()) {
					T aliveSuper = new AliveFinder().find(vertex.getSupers().get(0));
					System.out.println("isAlive, isMeta " + vertex.info() + ", aliveSuper : " + aliveSuper.info() + ", résultat : " + (aliveSuper != null ? getInheritings(aliveSuper).get(vertex) : null));
					return aliveSuper != null ? getInheritings(aliveSuper).get(vertex) : null;
				}
				T aliveMeta = new AliveFinder().find(vertex.getMeta());
				System.out.println("aliveMeta : " + aliveMeta + ", getInstances(aliveMeta).get(vertex) : " + getInstances(aliveMeta).get(vertex) + ", vertex : " + vertex);
				return aliveMeta != null ? getInstances(aliveMeta).get(vertex) : null;
			}
		}
		boolean result = vertex.equals(new AliveFinder().find(vertex));
		System.out.println("isAlive ? " + result);
		return /* vertex != null && */result;
	}

	default Snapshot<T> getInstances(T vertex) {
		return getDependencies(vertex).filter(Filters.INSTANCES, vertex);
	}

	default ObservableList<T> getObservableInstances(T vertex) {
		return getObservableDependencies(vertex).filtered(x -> vertex.equals(x.getMeta()));
	}

	default Snapshot<T> getInheritings(T vertex) {
		return getDependencies(vertex).filter(Filters.INHERITINGS, vertex);
	}

	default ObservableList<T> getObservableInheritings(T vertex) {
		return getObservableDependencies(vertex).filtered(x -> x.getSupers().contains(vertex));
	}

	default Snapshot<T> getComposites(T vertex) {
		return getDependencies(vertex).filter(Filters.COMPOSITES, vertex);
	}

	default ObservableList<T> getObservableComposites(T vertex) {
		return getObservableDependencies(vertex).filtered(x -> x.getComponents().contains(vertex));
	}

	default void discardWithException(Throwable exception) throws RollbackException {
		throw new RollbackException(exception);
	}

	default NavigableSet<T> computeDependencies(T node) {
		class OrderedDependencies extends TreeSet<T> {
			private static final long serialVersionUID = -441180182522681264L;

			OrderedDependencies visit(T node) {
				if (!contains(node)) {
					add(node);
					getDependencies(node).forEach(this::visit);
				}
				return this;
			}
		}
		return new OrderedDependencies().visit(node);
	}

	default NavigableSet<T> computePotentialDependencies(T meta, List<T> supers, Serializable value, List<T> components) {
		class PotentialDependenciesComputer extends TreeSet<T> {
			private static final long serialVersionUID = -4464199068092100672L;
			private final Set<T> alreadyVisited = new HashSet<>();

			PotentialDependenciesComputer visit(T node) {
				if (!alreadyVisited.contains(node))
					if (node.isDependencyOf(meta, supers, value, components))
						super.addAll(computeDependencies(node));
					else {
						alreadyVisited.add(node);
						getDependencies(node).forEach(this::visit);
					}
				return this;
			}
		}
		return new PotentialDependenciesComputer().visit(meta);
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

	ObservableList<T> getObservableDependencies(T generic);

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
