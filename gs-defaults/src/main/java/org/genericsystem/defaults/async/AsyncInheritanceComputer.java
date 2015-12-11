package org.genericsystem.defaults.async;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultVertex;

public class AsyncInheritanceComputer<T extends DefaultVertex<T>> extends HashSet<T> {

	private static final long serialVersionUID = 1877502935577170921L;

	private final Map<T, Collection<T>> inheritingsCache = new HashMap<>();
	private final Map<T, CompletableFuture<Collection<T>>> inheritingsPromiseCache = new HashMap<>();

	private final T base;
	private final T origin;
	private final int level;

	public AsyncInheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
	}

	public CompletableFuture<Stream<T>> inheritanceStreamAsync() {
		return getInheringsStreamAsync(base).thenApply(inherings -> {
			return inherings.filter(holder -> !contains(holder) && !holder.equals(origin) && holder.getLevel() == level);
		});
	}

	public Stream<T> inheritanceStreamSync() {
		try {
			return inheritanceStreamAsync().get(1000, TimeUnit.MILLISECONDS);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new IllegalStateException(e);
		}
	}

	public Stream<T> inheritanceStream() {
		return inheritanceStreamSync();
	}

	private CompletableFuture<Stream<T>> getInheringsStreamAsync(T superVertex) {
		CompletableFuture<Collection<T>> result = inheritingsPromiseCache.get(superVertex);
		if (result == null)
			inheritingsPromiseCache.put(superVertex, result = new Inheritings(superVertex).inheritanceStreamAsync().thenApply(stream -> stream.collect(Collectors.toList())));
		return result.thenApply(collection -> collection.stream());
	}

	private class Inheritings {

		private final T localBase;

		private Inheritings(T localBase) {
			this.localBase = localBase;
		}

		private CompletableFuture<Stream<T>> inheritanceStreamAsync() {
			return fromAboveStreamAsync().thenCompose(stream -> {
				CompletableFuture<Stream<T>> internal = CompletableFuture.completedFuture(Stream.empty());
				for (T holder : (Snapshot<T>) () -> stream)
					internal = internal.thenCompose(internalStream -> getStreamAsync(holder).thenApply(s -> Stream.concat(internalStream, s)));
				return internal.thenApply(s -> s.distinct());
			});
		}

		private boolean hasIntermediateSuperOrIsMeta() {
			return localBase.isMeta() || localBase.getSupers().stream().filter(next -> localBase.getMeta().equals(next.getMeta())).count() != 0;
		}

		private Stream<T> metaAndSupersStream() {
			return Stream.concat(hasIntermediateSuperOrIsMeta() ? Stream.empty() : Stream.of(localBase.getMeta()), localBase.getSupers().stream()).distinct();
		}

		// TODO InheritanceComputer.this not used

		private CompletableFuture<Stream<T>> fromAboveStreamAsync() {
			CompletableFuture<Stream<T>> streamPromise = new CompletableFuture<>();
			if (localBase.isRoot())
				streamPromise = CompletableFuture.completedFuture(Stream.of(origin));
			else {
				CompletableFuture<Stream<T>> internal = CompletableFuture.completedFuture(Stream.empty());
				for (T t : (Snapshot<T>) () -> metaAndSupersStream())
					internal = internal.thenCompose(stream -> getInheringsStreamAsync(t).thenApply(inherings -> Stream.concat(stream, inherings)));
				streamPromise = internal.thenApply(stream -> stream.distinct());
			}
			return streamPromise;
		}

		private CompletableFuture<Stream<T>> getStreamAsync(final T holder) {

			CompletableFuture<Stream<T>> internal = compositesBySuperAsync(localBase, holder).thenApply(compositesBySuper -> {
				if (compositesBySuper.count() != 0)
					add(holder);
				return Stream.empty();
			});

			internal = internal.thenCompose(unused -> {
				return compositesBySuperAsync(localBase, holder).thenCompose(compositesBySuper -> {
					CompletableFuture<Stream<T>> compositesByMetaPromise = holder.getLevel() < level ? compositesByMetaAsync(localBase, holder) : CompletableFuture.completedFuture(Stream.empty());
					return compositesByMetaPromise.thenApply(compositesByMeta -> Stream.concat(compositesByMeta, compositesBySuper));
				});
			});

			internal = internal.thenCompose(indexStream -> {
				CompletableFuture<Stream<T>> internalBis = CompletableFuture.completedFuture(Stream.empty());
				for (T t : (Snapshot<T>) () -> indexStream)
					internalBis = internalBis.thenCompose(stream -> getStreamAsync(t).thenApply(nextStream -> Stream.concat(stream, nextStream)));
				return internalBis;
			});

			return internal.thenApply(stream -> Stream.concat(Stream.of(holder), stream).distinct());
		}

	}

	private static <T extends DefaultVertex<T>> CompletableFuture<Stream<T>> compositesByMetaAsync(T localBase, T holder) {
		return localBase.getAsyncComposites().thenApply(snapshot -> snapshot.stream().filter(x -> !x.equals(holder) && x.getMeta().equals(holder)));
	}

	private static <T extends DefaultVertex<T>> CompletableFuture<Stream<T>> compositesBySuperAsync(T localBase, T holder) {
		return localBase.getAsyncComposites().thenApply(snapshot -> snapshot.stream().filter(x -> x.getSupers().contains(holder)));
	}
}
