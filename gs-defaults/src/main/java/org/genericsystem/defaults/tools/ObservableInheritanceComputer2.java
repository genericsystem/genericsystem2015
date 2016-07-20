package org.genericsystem.defaults.tools;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.defaults.DefaultGeneric;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public class ObservableInheritanceComputer2<T extends DefaultGeneric<T>> extends ListBinding<T> {

	private Set<Observable> invalidators = new HashSet<Observable>();;

	private final T base;
	private final T origin;
	private final int level;

	public ObservableInheritanceComputer2(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
	}

	public static int i = 0;

	@Override
	protected ObservableList<T> computeValue() {
		invalidators.forEach(this::unbind);
		invalidators = new HashSet<Observable>();
		List<T> internal = new InheritanceComputer<T>(base, origin, level) {
			private static final long serialVersionUID = 4269874133078234506L;

			@Override
			protected Inheritings buildInheritings(T superVertex) {
				return new Inheritings(superVertex) {
					@Override
					protected Stream<T> compositesByMeta(T holder) {
						ObservableList<T> ol = localBase.getObservableComposites();
						ol.addListener((InvalidationListener) c -> System.out.println("zzzzzzzzzzzzz" + i + " " + localBase + " " + holder));
						ObservableList<T> filtered = ol.filtered(x -> !x.equals(holder) && x.getMeta().equals(holder));
						filtered.addListener((InvalidationListener) c -> System.out.println("ffffffffffff" + i + " " + localBase + " " + holder));
						i++;
						invalidators.add(filtered);
						return filtered.stream();
					}

					@Override
					protected Stream<T> compositesBySuper(T holder) {
						ObservableList<T> ol = localBase.getObservableComposites();
						ObservableList<T> filtered = ol.filtered(x -> x.getSupers().contains(holder));
						invalidators.add(filtered);
						return filtered.stream();
					}
				};

			};
		}.inheritanceStream().collect(Collectors.toList());
		invalidators.forEach(this::bind);
		return FXCollections.unmodifiableObservableList(FXCollections.observableList(internal));
	}

}
