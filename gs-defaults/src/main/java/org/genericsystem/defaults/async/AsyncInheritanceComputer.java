package org.genericsystem.defaults.async;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultVertex;

public class AsyncInheritanceComputer<T extends DefaultVertex<T>> extends HashSet<T> {

	private static final long serialVersionUID = 1877502935577170921L;

	private final Map<T, CompletableFuture<Snapshot<T>>> inheritingsPromiseCache = new HashMap<>();

	private final T base;
	private final T origin;
	private final int level;

	public AsyncInheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
	}

	public CompletableFuture<Snapshot<T>> inheritanceStreamAsync() {
		return getInheringsStreamAsync(base).thenApply(inherings -> inherings.filter(holder -> !contains(holder) && !holder.equals(origin) && holder.getLevel() == level));
	}

	public Stream<T> inheritanceStreamSync() {
		try {
			return inheritanceStreamAsync().get(1000, TimeUnit.MILLISECONDS).stream();
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new IllegalStateException(e);
		}
	}

	public Stream<T> inheritanceStream() {
		return inheritanceStreamSync();
	}

	private CompletableFuture<Snapshot<T>> getInheringsStreamAsync(T superVertex) {
		CompletableFuture<Snapshot<T>> result = inheritingsPromiseCache.get(superVertex);
		if (result == null)
			inheritingsPromiseCache.put(superVertex, result = new Inheritings(superVertex).inheritanceStreamAsync());
		return result;
	}

	private class Inheritings {

		private final T localBase;

		private Inheritings(T localBase) {
			this.localBase = localBase;
		}

		private CompletableFuture<Snapshot<T>> inheritanceStreamAsync() {
			return fromAboveStreamAsync().thenCompose(snapshot -> {
				CompletableFuture<Snapshot<T>> internal = CompletableFuture.completedFuture(() -> Stream.empty());
				for (T holder : snapshot)
					internal = internal.thenCompose(internalSnapshot -> getStreamAsync(holder).thenApply(s -> () -> Stream.concat(internalSnapshot.stream(), s.stream())));
				return internal.thenApply(s -> () -> s.stream().distinct());
			});
		}

		private boolean hasIntermediateSuperOrIsMeta() {
			return localBase.isMeta() || localBase.getSupers().stream().filter(next -> localBase.getMeta().equals(next.getMeta())).count() != 0;
		}

		private Snapshot<T> metaAndSupersStream() {
			return () -> Stream.concat(hasIntermediateSuperOrIsMeta() ? Stream.empty() : Stream.of(localBase.getMeta()), localBase.getSupers().stream()).distinct();
		}

		// TODO InheritanceComputer.this not used

		private CompletableFuture<Snapshot<T>> fromAboveStreamAsync() {
			CompletableFuture<Snapshot<T>> streamPromise = new CompletableFuture<>();
			if (localBase.isRoot())
				streamPromise = CompletableFuture.completedFuture(() -> Stream.of(origin));
			else {
				CompletableFuture<Snapshot<T>> internal = CompletableFuture.completedFuture(() -> Stream.empty());
				for (T t : metaAndSupersStream())
					internal = internal.thenCompose(snapshot -> getInheringsStreamAsync(t).thenApply(inherings -> () -> Stream.concat(snapshot.stream(), inherings.stream())));
				streamPromise = internal.thenApply(snapshot -> () -> snapshot.stream().distinct());
			}
			return streamPromise;
		}

		private CompletableFuture<Snapshot<T>> getStreamAsync(final T holder) {

			CompletableFuture<Snapshot<T>> internal = compositesBySuperAsync(localBase, holder).thenApply(compositesBySuper -> {
				if (compositesBySuper.count() != 0)
					add(holder);
				return () -> Stream.empty();
			});

			internal = internal.thenCompose(unused -> {
				return compositesBySuperAsync(localBase, holder).thenCompose(compositesBySuper -> {
					CompletableFuture<Snapshot<T>> compositesByMetaPromise = holder.getLevel() < level ? compositesByMetaAsync(localBase, holder).thenApply(s -> () -> s) : CompletableFuture.completedFuture(() -> Stream.empty());
					return compositesByMetaPromise.thenApply(compositesByMeta -> () -> Stream.concat(compositesByMeta.stream(), compositesBySuper));
				});
			});

			internal = internal.thenCompose(indexSnapshot -> {
				CompletableFuture<Snapshot<T>> internalBis = CompletableFuture.completedFuture(() -> Stream.empty());
				for (T t : indexSnapshot)
					internalBis = internalBis.thenCompose(snapshot -> getStreamAsync(t).thenApply(nextStream -> () -> Stream.concat(snapshot.stream(), nextStream.stream())));
				return internalBis;
			});

			return internal.thenApply(snapshot -> () -> Stream.concat(Stream.of(holder), snapshot.stream()).distinct());
		}
	}

	private static <T extends DefaultVertex<T>> CompletableFuture<Stream<T>> compositesByMetaAsync(T localBase, T holder) {
		return localBase.getAsyncComposites().thenApply(snapshot -> snapshot.stream().filter(x -> !x.equals(holder) && x.getMeta().equals(holder)));
	}

	private static <T extends DefaultVertex<T>> CompletableFuture<Stream<T>> compositesBySuperAsync(T localBase, T holder) {
		return localBase.getAsyncComposites().thenApply(snapshot -> snapshot.stream().filter(x -> x.getSupers().contains(holder)));
	}
}
