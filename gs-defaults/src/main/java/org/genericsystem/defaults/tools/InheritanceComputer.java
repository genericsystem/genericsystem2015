package org.genericsystem.defaults.tools;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultGeneric;

import io.reactivex.Observable;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public class InheritanceComputer<T extends DefaultGeneric<T>> {

	private final Map<T, Collection<T>> inheritingsCache = new HashMap<>();
	private final Map<T, Observable<T>> addsCache = new HashMap<>();
	private final Map<T, Observable<T>> removesCache = new HashMap<>();
	private final Set<T> overridden = new HashSet<>();

	private final T base;
	private final T origin;
	private final int level;
	private final Predicate<T> inheritingsFilter;

	public InheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
		inheritingsFilter =  holder -> !overridden.contains(holder) && !holder.equals(origin) && holder.getLevel() == level;
	}

	public Stream<T> inheritanceStream() {
		return getInheritingsStream(base).filter(holder -> inheritingsFilter.test(holder));
	}

	public Observable<T> getAddsObservable() {
		return getAddsObservable(base).filter(holder -> inheritingsFilter.test(holder)).replay().refCount();
	}

	public Observable<T> getRemovesObservable() {
		return getRemovesObservable(base).filter(holder -> inheritingsFilter.test(holder)).replay().refCount();
	}

	private Stream<T> getInheritingsStream(T superVertex) {
		Collection<T> result = inheritingsCache.get(superVertex);
		if (result == null)
			inheritingsCache.put(superVertex, result = buildInheritings(superVertex).inheritanceStream().collect(Collectors.toList()));
		return result.stream();
		// return new Inheritings(superVertex).inheritanceStream();
	}

	private Observable<T> getAddsObservable(T superVertex) {
		Observable<T> result = addsCache.get(superVertex);
		if (result == null)
			addsCache.put(superVertex, result = buildInheritings(superVertex).inheritanceStreamAdds());
		return result;
	}

	private Observable<T> getRemovesObservable(T superVertex) {
		Observable<T> result = removesCache.get(superVertex);
		if (result == null)
			removesCache.put(superVertex, result = buildInheritings(superVertex).inheritanceStreamRemoves());
		return result;
	}

	protected Inheritings buildInheritings(T superVertex) {
		return new Inheritings(superVertex);
	}

	protected class Inheritings {

		protected final T localBase;

		protected Inheritings(T localBase) {
			this.localBase = localBase;
		}

		private boolean hasIntermediateSuperOrIsMeta() {
			return localBase.isMeta() || localBase.getSupers().stream().filter(next -> localBase.getMeta().equals(next.getMeta())).count() != 0;
		}

		private Stream<T> metaAndSupersStream() {
			return Stream.concat(hasIntermediateSuperOrIsMeta() ? Stream.empty() : Stream.of(localBase.getMeta()), localBase.getSupers().stream()).distinct();
		}

		private Stream<T> inheritanceStream() {
			return fromAboveStream().flatMap(holder -> getStream(holder)).distinct();
		}

		private Observable<T> inheritanceStreamAdds() {
			return Observable.merge(fromAboveAdds().flatMap(holder -> getObservable(getStream(holder))),
					Observable.merge(fromAboveAdds(), getObservable(fromAboveStream())).flatMap(holder -> getStreamAdds(holder))).replay().refCount();
		}

		private Observable<T> inheritanceStreamRemoves() {
			return Observable.merge(Observable.merge(fromAboveAdds(), getObservable(fromAboveStream())).flatMap(holder -> getStreamRemoves(holder)),
					fromAboveRemoves().flatMap(holder -> getObservable(getStream(holder)))).replay().refCount();
		}

		private Stream<T> fromAboveStream() {
			return localBase.isRoot() ? Stream.of(origin) : metaAndSupersStream().flatMap(InheritanceComputer.this::getInheritingsStream).distinct();
		}

		private Observable<T> fromAboveAdds() {
			return localBase.isRoot() ? Observable.never() : getObservable(metaAndSupersStream()).flatMap(InheritanceComputer.this::getAddsObservable).replay().refCount();
		}

		private Observable<T> fromAboveRemoves() {
			return localBase.isRoot() ? Observable.never() : getObservable(metaAndSupersStream()).flatMap(InheritanceComputer.this::getRemovesObservable).replay().refCount();
		}

		private Stream<T> getIndexStream(T holder) {
			return Stream.concat(holder.getLevel() < level ? compositesByMeta(holder).stream() : Stream.empty(), compositesBySuper(holder).stream());
		}

		private Observable<T> getIndexStreamAdds(T holder) {
			return Observable.merge(holder.getLevel() < level ? compositesByMeta(holder).getAddsObservable() : Observable.never(), compositesBySuper(holder).getAddsObservable()).replay().refCount();
		}

		private Observable<T> getIndexStreamRemoves(T holder) {
			return Observable.merge(holder.getLevel() < level ? compositesByMeta(holder).getRemovesObservable() : Observable.never(), compositesBySuper(holder).getRemovesObservable()).replay().refCount();
		}

		private Stream<T> getStream(final T holder) {
			if (compositesBySuper(holder).stream().count() != 0)
				overridden.add(holder);
			return Stream.concat(Stream.of(holder), getIndexStream(holder).flatMap(x -> getStream(x)).distinct());
		}

		private Observable<T> getStreamAdds(T holder) {
			Observable<T> indexAdds = getIndexStreamAdds(holder);
			return Observable.merge(Observable.merge(getObservable(getIndexStream(holder)), indexAdds).flatMap(x -> getStreamAdds(x)),
					indexAdds.flatMap(x -> getObservable(getStream(x)))).replay().refCount();
		}

		private Observable<T> getStreamRemoves(T holder) {
			return Observable.merge(getIndexStreamRemoves(holder).flatMap(x -> getObservable(getStream(x))),
					Observable.merge(getObservable(getIndexStream(holder)), getIndexStreamAdds(holder)).flatMap(x -> getStreamRemoves(x))).replay().refCount();
		}

		private Snapshot<T> compositesByMeta(T holder) {
			return localBase.getDependencies().filter(Arrays.asList(new IndexFilter(FiltersBuilder.COMPOSITES, localBase), new IndexFilter(FiltersBuilder.HAS_META, holder)));
		}

		private Snapshot<T> compositesBySuper(T holder) {
			return localBase.getDependencies().filter(Arrays.asList(new IndexFilter(FiltersBuilder.COMPOSITES, localBase), new IndexFilter(FiltersBuilder.HAS_SUPER, holder)));
		}

		private Observable<T> getObservable(Stream<T> stream) {
			return Observable.fromIterable(stream.collect(Collectors.toList())).replay().refCount();
		}
	}
}
