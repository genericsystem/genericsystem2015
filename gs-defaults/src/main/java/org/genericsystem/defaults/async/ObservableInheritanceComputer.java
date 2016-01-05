package org.genericsystem.defaults.async;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.ObservableList;

import org.genericsystem.defaults.DefaultVertex;

import com.sun.javafx.collections.ObservableListWrapper;

@SuppressWarnings("restriction")
public class ObservableInheritanceComputer<T extends DefaultVertex<T>> extends HashSet<T> {

	private static final long serialVersionUID = 5848697526461046720L;

	private final Map<T, Collection<T>> inheritingsCache = new HashMap<>();

	private final T base;
	private final T origin;
	private final int level;
	private InheritenceComputerBinding binding;

	public ObservableInheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;

	}

	public ObservableList<T> observableInheritanceList() {
		if (binding == null)
			binding = new InheritenceComputerBinding();
		return binding;
	}

	private class InheritenceComputerBinding extends ListBinding<T> {

		Set<ObservableList<?>> observables = new HashSet<>();

		public void toBind(ObservableList<?> observable) {
			observables.add(observable);
			bind(observable);
		}

		private void unbindAll() {
			for (Observable observable : observables)
				unbind(observable);
			observables.clear();
		}

		@Override
		protected ObservableList<T> computeValue() {
			unbindAll();
			inheritingsCache.clear();
			ObservableInheritanceComputer.this.clear();

			List<T> list = getInheringsStream(base).filter(holder -> !ObservableInheritanceComputer.this.contains(holder) && !holder.equals(origin) && holder.getLevel() == level).collect(Collectors.toList());

			return new ObservableListWrapper<>(list);
		}
	}

	private Stream<T> getInheringsStream(T superVertex) {
		Collection<T> result = inheritingsCache.get(superVertex);
		if (result == null)
			inheritingsCache.put(superVertex, result = new Inheritings(superVertex).inheritanceStream().collect(Collectors.toList()));
		return result.stream();
		// return new Inheritings(superVertex).inheritanceStream().collect(Collectors.toList()));
	}

	private class Inheritings {

		private final T localBase;

		private Inheritings(T localBase) {
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
			return localBase.isRoot() ? Stream.of(origin) : metaAndSupersStream().flatMap(ObservableInheritanceComputer.this::getInheringsStream).distinct();
		}

		private Stream<T> getStream(final T holder) {
			if (compositesBySuper(localBase, holder).count() != 0)
				add(holder);
			Stream<T> indexStream = Stream.concat(holder.getLevel() < level ? compositesByMeta(localBase, holder) : Stream.empty(), compositesBySuper(localBase, holder));
			return Stream.concat(Stream.of(holder), indexStream.flatMap(x -> getStream(x)).distinct());
		}
	}

	@SuppressWarnings("hiding")
	private <T extends DefaultVertex<T>> Stream<T> compositesByMeta(T localBase, T holder) {
		binding.toBind(localBase.getCurrentCache().getObservableDependencies(localBase));
		return localBase.getComposites().stream().filter(x -> !x.equals(holder) && x.getMeta().equals(holder));
	}

	@SuppressWarnings("hiding")
	private <T extends DefaultVertex<T>> Stream<T> compositesBySuper(T localBase, T holder) {
		binding.toBind(localBase.getCurrentCache().getObservableDependencies(localBase));
		return localBase.getComposites().stream().filter(x -> x.getSupers().contains(holder));
	}
}
