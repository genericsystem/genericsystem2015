package org.genericsystem.distributed.cacheonclient;

import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.stream.Stream;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Differential;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

public class AsyncDifferential extends Differential implements AsyncIDifferential {

	public AsyncDifferential(AsyncIDifferential subCache) {
		super(subCache);
	}

	@Override
	public AsyncIDifferential getSubCache() {
		return (AsyncIDifferential) super.getSubCache();
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
	public ObservableValue<CompletableFuture<Snapshot<Generic>>> getDependenciesPromise(Generic generic) {

		return new ObjectBinding<CompletableFuture<Snapshot<Generic>>>() {

			private ObservableSnapshot<Generic> addsSnapFilter = addsSnap.filtered(t -> generic.isDirectAncestorOf(t));
			private ObservableSnapshot<Generic> removesSnapFilter = removesSnap.filtered(t -> generic.isDirectAncestorOf(t));
			private ObservableValue<CompletableFuture<Snapshot<Generic>>> dependenciesPromise = getSubCache().getDependenciesPromise(generic);
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
		return getSubCache().getDependenciesObservableSnapshot(generic).filtered(removePredicate).concat(addsSnap.filtered(x -> generic.isDirectAncestorOf(x)));
	}
}
