package org.genericsystem.defaults.tools;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.Filters;
import org.genericsystem.defaults.DefaultGeneric;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public class InheritanceComputer<T extends DefaultGeneric<T>> extends HashSet<T> {

	private static final long serialVersionUID = 1877502935577170921L;

	private final Map<T, Collection<T>> inheritingsCache = new HashMap<>();

	private final T base;
	private final T origin;
	private final int level;

	public InheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
	}

	public Stream<T> inheritanceStream() {
		return getInheringsStream(base).filter(holder -> !contains(holder) && !holder.equals(origin) && holder.getLevel() == level);
	}

	private Stream<T> getInheringsStream(T superVertex) {
		Collection<T> result = inheritingsCache.get(superVertex);
		if (result == null)
			inheritingsCache.put(superVertex, result = buildInheritings(superVertex).inheritanceStream().collect(Collectors.toList()));
		return result.stream();
		// return new Inheritings(superVertex).inheritanceStream();
	}

	protected Inheritings buildInheritings(T superVertex) {
		return new Inheritings(superVertex);
	}

	protected class Inheritings {

		protected final T localBase;

		protected Inheritings(T localBase) {
			this.localBase = localBase;
		}

		private Stream<T> inheritanceStream() {
			return fromAboveStream().flatMap(holder -> getStream(holder)).distinct();
		}

		private boolean hasIntermediateSuperOrIsMeta() {
			return localBase.isMeta() || localBase.getSupers().stream().filter(next -> localBase.getMeta().equals(next.getMeta())).count() != 0;
		}

		private Stream<T> metaAndSupersStream() {
			return Stream.concat(hasIntermediateSuperOrIsMeta() ? Stream.empty() : Stream.of(localBase.getMeta()), localBase.getSupers().stream()).distinct();
		}

		private Stream<T> fromAboveStream() {
			return localBase.isRoot() ? Stream.of(origin) : metaAndSupersStream().flatMap(InheritanceComputer.this::getInheringsStream).distinct();
		}

		private Stream<T> getStream(final T holder) {
			if (compositesBySuper(holder).count() != 0)
				add(holder);
			Stream<T> indexStream = Stream.concat(holder.getLevel() < level ? compositesByMeta(holder) : Stream.empty(), compositesBySuper(holder));
			return Stream.concat(Stream.of(holder), indexStream.flatMap(x -> getStream(x)).distinct());
		}

		@SuppressWarnings("unchecked")
		protected Stream<T> compositesByMeta(T holder) {
			return localBase.getDependencies().filter(Filters.COMPOSITES_BY_META, localBase, holder).stream(); // getComposites().stream().filter(x -> !x.equals(holder) && x.getMeta().equals(holder));
		}

		@SuppressWarnings("unchecked")
		protected Stream<T> compositesBySuper(T holder) {
			return localBase.getDependencies().filter(Filters.COMPOSITES_BY_SUPER, localBase, holder).stream(); // getComposites().stream().filter(x -> x.getSupers().contains(holder));
		}

	}

}
