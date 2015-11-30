package org.genericsystem.distributed.cacheonclient;

import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.stream.Stream;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.ObservableValueBase;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Differential;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.observables.ContainerObservableSnapshot;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

public class AsyncDifferential extends Differential implements AsyncIDifferential {

	private final ContainerObservableSnapshot<Generic> addsSnap = new ContainerObservableSnapshot<>();
	private final ContainerObservableSnapshot<Generic> removesSnap = new ContainerObservableSnapshot<>();

	public AsyncDifferential(AsyncIDifferential subCache) {
		super(subCache);
	}

	@Override
	public AsyncIDifferential getSubDifferential() {
		return (AsyncIDifferential) super.getSubDifferential();
	}

	@Override
	protected Generic plug(Generic generic) {
		super.plug(generic);
		addsSnap.add(generic);
		return generic;
	}

	@Override
	protected void unplug(Generic generic) {
		super.unplug(generic);

		if (!addsSnap.remove(generic))
			removesSnap.add(generic);
	}

	@Override
	public ObservableValue<CompletableFuture<Snapshot<Generic>>> getObervableDependenciesPromise(Generic generic) {

		return new ObjectBinding<CompletableFuture<Snapshot<Generic>>>() {

			private ObservableSnapshot<Generic> addsSnapFilter = addsSnap.filtered(t -> generic.isDirectAncestorOf(t));
			private ObservableSnapshot<Generic> removesSnapFilter = removesSnap.filtered(t -> generic.isDirectAncestorOf(t));
			private ObservableValue<CompletableFuture<Snapshot<Generic>>> dependenciesPromise = getSubDifferential().getObervableDependenciesPromise(generic);
			{
				bind(addsSnapFilter);
				bind(removesSnapFilter);
				bind(dependenciesPromise);
			}

			@Override
			protected void onInvalidating() {
				System.out.println("(AsyncDifferential - getDependenciesPromise) Invalidation");
			}

			@Override
			protected CompletableFuture<Snapshot<Generic>> computeValue() {
				return dependenciesPromise.getValue().<Snapshot<Generic>> thenApply(snapshot -> new Snapshot<Generic>() {

					@Override
					public Generic get(Object o) {
						Generic result = addsSnap.get(o);
						if (result != null)
							return generic.isDirectAncestorOf(result) ? result : null;
						return !removesSnap.contains(o) ? snapshot.get(o) : null;
					}

					@Override
					public Stream<Generic> stream() {
						return Stream.concat(addsSnap.contains(generic) ? Stream.empty() : snapshot.stream().filter(x -> !removesSnap.contains(x)), addsSnap.stream().filter(x -> generic.isDirectAncestorOf(x)));
					}
				});
			}
		};
	}

	@Override
	public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic) {
		ObservableValue<Predicate<Generic>> removePredicate = Bindings.<Predicate<Generic>> createObjectBinding(() -> t -> !removesSnap.contains(t), removesSnap);
		return getSubDifferential().getDependenciesObservableSnapshot(generic).filtered(removePredicate).concat(addsSnap.filtered(x -> generic.isDirectAncestorOf(x)));
	}

	private static class Invalidator<T> extends ObservableValueBase<T> implements InvalidationListener, Observable {

		public static <T> Invalidator<T> createInvalidator(Observable... observables) {
			return new Invalidator<T>(observables);
		}

		private Invalidator(Observable... observables) {
			for (Observable observable : observables)
				observable.addListener(new WeakInvalidationListener(this));
		}

		@Override
		public void invalidated(Observable observable) {
			super.fireValueChangedEvent();
		}

		@Override
		public T getValue() {
			return null;
		}

	}

	@Override
	public Observable getInvalidator(Generic generic) {
		return Invalidator.createInvalidator(getSubDifferential().getInvalidator(generic), adds.getFilteredInvalidator(generic, generic::isDirectAncestorOf), removes.getFilteredInvalidator(generic, generic::isDirectAncestorOf));
	}

	@Override
	public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic) {
		return getSubDifferential().getDependenciesPromise(generic).<Snapshot<Generic>> thenApply(subSnapshot -> new Snapshot<Generic>() {
			@Override
			public Generic get(Object o) {
				Generic result = adds.get(o);
				if (result != null)
					return generic.isDirectAncestorOf(result) ? result : null;
				return !removes.contains(o) ? subSnapshot.get(o) : null;
			}

			// @Override
			// public int size() {
			// return backingSet.size() + backingSet2.size();
			// }
			//
			// @Override
			// public E get(int index) {
			// return index < backingSet.size() ? backingSet.get(index) : backingSet2.get(index - backingSet.size());
			// }
			// TODO size and get(index) !!!

			@Override
			public Stream<Generic> stream() {
				return Stream.concat(adds.contains(generic) ? Stream.empty() : subSnapshot.stream().filter(x -> !removes.contains(x)), adds.stream().filter(x -> generic.isDirectAncestorOf(x)));
			}
		});
	}
}
