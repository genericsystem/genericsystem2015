package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CompletableFuture;

import javafx.beans.Observable;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IContext;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.api.core.exceptions.UnreachableOverridesException;
import org.genericsystem.defaults.async.AsyncSupersComputer;

public interface DefaultContext<T extends DefaultVertex<T>> extends IContext<T> {

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

	default CompletableFuture<Boolean> isAsyncAlive(T vertex) {
		assert vertex != null;
		class AliveFinder {
			CompletableFuture<T> find(T vertex) {
				if (vertex.isRoot())
					return CompletableFuture.completedFuture(vertex);
				if (vertex.isMeta()) {
					CompletableFuture<T> aliveSuperPromise = new AliveFinder().find(vertex.getSupers().get(0));
					return aliveSuperPromise.thenCompose(aliveSuper -> aliveSuper != null ? getAsyncInheritings(aliveSuper).thenApply(inherintings -> inherintings.get(vertex)) : CompletableFuture.completedFuture(null));
				}
				CompletableFuture<T> aliveMetaPromise = new AliveFinder().find(vertex.getMeta());

				return aliveMetaPromise.thenCompose(aliveMeta -> aliveMeta != null ? getAsyncInstances(aliveMeta).thenApply(x -> x.get(vertex)) : CompletableFuture.completedFuture(null));
			}
		}
		return new AliveFinder().find(vertex).thenApply(x -> /* vertex != null && */vertex.equals(x));
	}

	default Snapshot<T> getInstances(T vertex) {
		return getDependencies(vertex).filter(x -> vertex.equals(x.getMeta()));
	}

	default CompletableFuture<Snapshot<T>> getAsyncInstances(T vertex) {
		return getDependenciesPromise(vertex).thenApply(f -> f.filter(x -> vertex.equals(x.getMeta())));
	}

	default ObservableList<T> getObservableInstances(T vertex) {
		return getObservableDependencies(vertex).filtered(x -> vertex.equals(x.getMeta()));
	}

	default Snapshot<T> getInheritings(T vertex) {
		return getDependencies(vertex).filter(x -> x.getSupers().contains(vertex));
	}

	default CompletableFuture<Snapshot<T>> getAsyncInheritings(T vertex) {
		return getDependenciesPromise(vertex).thenApply(f -> f.filter(x -> x.getSupers().contains(vertex)));
	}

	default ObservableList<T> getObservableInheritings(T vertex) {
		return getObservableDependencies(vertex).filtered(x -> x.getSupers().contains(vertex));
	}

	default Snapshot<T> getComposites(T vertex) {
		return getDependencies(vertex).filter(x -> x.getComponents().contains(vertex));
	}

	default CompletableFuture<Snapshot<T>> getAsyncComposites(T vertex) {
		return getDependenciesPromise(vertex).thenApply(f -> f.filter(x -> x.getComponents().contains(vertex)));
	}

	default ObservableList<T> getObservableComposites(T vertex) {
		return getObservableDependencies(vertex).filtered(x -> x.getComponents().contains(vertex));
	}

	default void discardWithException(Throwable exception) throws RollbackException {
		throw new RollbackException(exception);
	}

	default Exception discardWithExceptionPromise(Throwable exception) throws RollbackException {
		return new RollbackException(exception);
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

	/**
	 * 
	 * TODO A TESTER
	 */
	default CompletableFuture<NavigableSet<T>> computeAsyncDependencies(T node) {
		class OrderedDependencies extends TreeSet<T> {
			private static final long serialVersionUID = -8377068939538941700L;

			public CompletableFuture<NavigableSet<T>> traverse(T generic) {
				if (!contains(generic)) {
					add(generic);
					CompletableFuture<NavigableSet<T>> cf = new CompletableFuture<NavigableSet<T>>();
					getDependenciesPromise(generic).thenAccept(snapshot -> {
						CompletableFuture<NavigableSet<T>> internal = CompletableFuture.completedFuture(this);
						for (T element : snapshot)
							internal = internal.thenCompose((a) -> traverse(element));
						internal.thenRun(() -> cf.complete(this));
					});
					return cf;
				}
				return CompletableFuture.completedFuture(this);
			}
		}

		return new OrderedDependencies().traverse(node);
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

	/**
	 * 
	 * TODO A TESTER
	 */
	default CompletableFuture<NavigableSet<T>> computeAsyncPotentialDependencies(T meta, List<T> supers, Serializable value, List<T> components) {
		class PotentialDependenciesComputer extends TreeSet<T> {
			private static final long serialVersionUID = -8377068939538941700L;
			private final Set<T> alreadyVisited = new HashSet<>();

			public CompletableFuture<NavigableSet<T>> traverse(T node) {
				final CompletableFuture<NavigableSet<T>> cf = new CompletableFuture<NavigableSet<T>>();

				if (alreadyVisited.contains(node))
					cf.complete(this);
				else {
					if (node.isDependencyOf(meta, supers, value, components))
						computeAsyncDependencies(node).thenAccept(dep -> super.addAll(dep));
					else {
						alreadyVisited.add(node);
						getDependenciesPromise(node).thenAccept(snapshot -> {
							CompletableFuture<NavigableSet<T>> internal = CompletableFuture.completedFuture(this);
							for (T element : snapshot)
								internal = internal.thenCompose((a) -> traverse(element));
							internal.thenRun(() -> cf.complete(this));
						});

					}
				}
				return cf;
			}
		}

		return new PotentialDependenciesComputer().traverse(meta);
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

	/**
	 * 
	 * TODO A TESTER
	 */
	default CompletableFuture<NavigableSet<T>> computeAsyncRemoveDependencies(T node) {
		class OrderedRemoveDependencies extends TreeSet<T> {
			private static final long serialVersionUID = -5970021419012502402L;

			CompletableFuture<NavigableSet<T>> traverse(T node) {
				CompletableFuture<NavigableSet<T>> cf = new CompletableFuture<NavigableSet<T>>();
				if (contains(node))
					cf.complete(this);
				else {
					CompletableFuture<NavigableSet<T>> status;
					status = getAsyncInheritings(node).thenCompose(inheritingsDependencies -> {
						CompletableFuture<NavigableSet<T>> internal = CompletableFuture.completedFuture(this);
						if (!inheritingsDependencies.isEmpty()) {
							cf.completeExceptionally(discardWithExceptionPromise(new ReferentialIntegrityConstraintViolationException("Ancestor : " + node + " has a inheriting dependencies : " + inheritingsDependencies.info())));
						}
						for (T element : inheritingsDependencies)
							internal = internal.thenCompose((a) -> traverse(element));
						return internal;
					});
					status = status.thenCompose((a) -> getAsyncInstances(node).thenCompose(instancesDependencies -> {
						CompletableFuture<NavigableSet<T>> internal = CompletableFuture.completedFuture(this);
						if (!instancesDependencies.isEmpty())
							cf.completeExceptionally(discardWithExceptionPromise(new ReferentialIntegrityConstraintViolationException("Ancestor : " + node + " has a instance dependencies : " + instancesDependencies.info())));
						for (T element : instancesDependencies)
							internal = internal.thenCompose((b) -> traverse(element));
						return internal;
					}));
					status = status.thenCompose((a) -> getAsyncComposites(node).thenCompose(compositesDependencies -> {
						CompletableFuture<NavigableSet<T>> internal = CompletableFuture.completedFuture(this);
						for (T composite : compositesDependencies) {
							for (int componentPos = 0; componentPos < composite.getComponents().size(); componentPos++)
								if (!composite.getComponents().get(componentPos).equals(node) && !contains(composite) && composite.getMeta().isReferentialIntegrityEnabled(componentPos))
									cf.completeExceptionally(discardWithExceptionPromise(new ReferentialIntegrityConstraintViolationException(composite + " is Referential Integrity for ancestor " + node + " by composite position : " + componentPos)));
							internal = internal.thenCompose((b) -> traverse(composite));
						}
						return internal;
					}));
					add(node);
					for (int axe = 0; axe < node.getComponents().size(); axe++) {
						if (node.isCascadeRemoveEnabled(axe)) {
							int axeFix = axe;
							status = status.thenCompose((a) -> traverse(node.getComponents().get(axeFix)));
						}
					}
					status.thenRun(() -> cf.complete(this));
				}

				return cf;
			}
		}
		return new OrderedRemoveDependencies().traverse(node);
	}

	default List<T> computeAndCheckOverridesAreReached(T adjustedMeta, List<T> overrides, Serializable value, List<T> components) {
		List<T> supers = new ArrayList<>(new SupersComputer<>(adjustedMeta, overrides, value, components));
		if (!ApiStatics.areOverridesReached(supers, overrides))
			discardWithException(new UnreachableOverridesException("Unable to reach overrides : " + overrides + " with computed supers : " + supers));
		return supers;
	}

	default CompletableFuture<List<T>> computeAsyncAndCheckOverridesAreReached(T adjustedMeta, List<T> overrides, Serializable value, List<T> components) {
		CompletableFuture<List<T>> cf = new CompletableFuture<>();
		new AsyncSupersComputer<T>(adjustedMeta, overrides, value, components).get().thenAccept(list -> {
			List<T> supers = new ArrayList<>(list);
			if (!ApiStatics.areOverridesReached(supers, overrides))
				cf.completeExceptionally(discardWithExceptionPromise(new UnreachableOverridesException("Unable to reach overrides : " + overrides + " with computed supers : " + supers)));
			cf.complete(supers);
		});
		return cf;
	}

	Snapshot<T> getDependencies(T vertex);

	CompletableFuture<Snapshot<T>> getDependenciesPromise(T vertex);

	ObservableList<T> getObservableDependencies(T generic);

	Observable getInvalidator(T generic);

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
