package org.genericsystem.defaults.tools;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.defaults.DefaultGeneric;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */

public class ObservableInheritanceComputer<T extends DefaultGeneric<T>> {

	private final Map<T, Collection<T>> inheritingsCache = new HashMap<>();

	private final Set<T> set = new HashSet<T>();

	private final T base;
	private final T origin;
	private final int level;
	private final InheritanceComputerBinding binding;

	public ObservableInheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
		binding = new InheritanceComputerBinding();
	}

	public ObservableList<T> observableInheritanceList() {
		return binding;
	}

	private class InheritanceComputerBinding extends ListBinding<T> {

		Set<Observable> observables = new HashSet<>();

		public void toBind(Observable observable) {
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
			set.clear();
			return FXCollections.unmodifiableObservableList(FXCollections.observableList((getInheringsStream(base).filter(
					holder -> !set.contains(holder) && !holder.equals(origin) && holder.getLevel() == level).collect(Collectors.toList()))));
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
				set.add(holder);
			Stream<T> indexStream = Stream.concat(holder.getLevel() < level ? compositesByMeta(localBase, holder) : Stream.empty(),
					compositesBySuper(localBase, holder));
			return Stream.concat(Stream.of(holder), indexStream.flatMap(x -> getStream(x)).distinct());
		}
	}

	@SuppressWarnings("hiding")
	private <T extends DefaultGeneric<T>> Stream<T> compositesByMeta(T localBase, T holder) {
		ObservableList<T> composites = localBase.getObservableComposites().filtered(x -> !x.equals(holder) && x.getMeta().equals(holder));
		binding.toBind(composites);
		return composites.stream();
	}

	@SuppressWarnings("hiding")
	private <T extends DefaultGeneric<T>> Stream<T> compositesBySuper(T localBase, T holder) {
		ObservableList<T> composites = localBase.getObservableComposites().filtered(x -> x.getSupers().contains(holder));
		binding.toBind(composites);
		return composites.stream();
	}
}
