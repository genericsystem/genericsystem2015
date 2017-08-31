package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IndexFilter;
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
public class ObservableInheritanceComputer<T extends DefaultGeneric<T>> extends ListBinding<T> {

	List<Observable> invalidators = new ArrayList<>();

	private final T base;
	private final T origin;
	private final int level;

	public ObservableInheritanceComputer(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
	}

	@Override
	protected ObservableList<T> computeValue() {
		invalidators.forEach(ObservableInheritanceComputer.this::unbind);
		List<Observable> newInvalidators = new ArrayList<>();
		List<T> internal = new InheritanceComputer<T>(base, origin, level) {

			@Override
			protected Inheritings buildInheritings(T superVertex) {
				return new Inheritings(superVertex) {
					@Override
					protected Stream<T> compositesByMeta(T holder) {
						ObservableList<T> filtered = localBase.getComposites().filter(new IndexFilter(FiltersBuilder.HAS_META, holder)).toObservableList();
						newInvalidators.add(filtered);
						bind(filtered);
						return super.compositesByMeta(holder);
					}

					@Override
					protected Stream<T> compositesBySuper(T holder) {
						ObservableList<T> filtered = localBase.getComposites().filter(new IndexFilter(FiltersBuilder.HAS_SUPER, holder)).toObservableList();
						newInvalidators.add(filtered);
						bind(filtered);
						return super.compositesBySuper(holder);
					}
				};

			};
		}.inheritanceStream().collect(Collectors.toList());
		invalidators = newInvalidators;
		return FXCollections.observableList(internal);
	}
}
