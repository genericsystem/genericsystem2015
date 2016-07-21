
package org.genericsystem.defaults.tools;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
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

	private Set<Observable> invalidators = new HashSet<Observable>();
	// private static Set<Observable> refs = new HashSet<Observable>();

	private final T base;
	private final T origin;
	private final int level;
	private boolean out = false;
	List<T> internal;

	public ObservableInheritanceComputer2(T base, T origin, int level) {
		this.base = base;
		this.origin = origin;
		this.level = level;
		if (Objects.toString(base.getValue()).contains("Yellow") && Objects.toString(origin.getValue()).contains("CarColor")) {
			System.out.println("Register ObservableInheritanceComputer2 on Yellow");
			out = true;
			addListener((InvalidationListener) (c -> System.out.println("ObservableInheritanceComputer2 on Yellow has changed : " + c)));
		}
	}

	// @Override
	// protected void onInvalidating() {
	// if (out)
	// System.out.println("Invalidation of ObservableInheritanceComputer2 on Yellow");
	// super.onInvalidating();
	// }

	// public static int i = 0;

	@Override
	protected ObservableList<T> computeValue() {
		invalidators.forEach(ObservableInheritanceComputer2.this::unbind);
		invalidators = new HashSet<Observable>();
		internal = new InheritanceComputer<T>(base, origin, level) {
			private static final long serialVersionUID = 4269874133078234506L;

			@Override
			protected Inheritings buildInheritings(T superVertex) {
				return new Inheritings(superVertex) {
					@Override
					protected Stream<T> compositesByMeta(T holder) {
						ObservableList<T> ol = localBase.getObservableComposites();
						// boolean b = Objects.toString(localBase.getValue()).contains("Yellow") && Objects.toString(holder.getValue()).contains("CarColor");
						ObservableList<T> filtered = ol.filtered(x -> {
							boolean result = !x.equals(holder) && x.getMeta().equals(holder);
							// if (b)
							// System.out.println("FILTERRRRRRRRRRRRRRRRRRR" + result);
							return result;
						});
						// if (b) {
						// System.out.println("REGISTER FILTERED LISTENER");
						// ol.addListener((InvalidationListener) c -> System.out.println("Composites of yellow" + i + " " + localBase + " " + holder));
						// ol.addListener((ListChangeListener) c -> System.out.println("Composites of yellow" + i + " " + localBase + " " + holder));
						// filtered.addListener((InvalidationListener) c -> System.out.println("Filtered composites of yellow" + i + " " + localBase + " " + holder));
						// filtered.addListener((ListChangeListener) c -> System.out.println("Filtered composites of yellow" + i + " " + localBase + " " + holder));
						// }
						// i++;
						// refs.add(filtered);
						// refs.add(ol);
						invalidators.add(filtered);
						bind(filtered);
						// ol.addListener((ListChangeListener) c -> ObservableInheritanceComputer2.this.invalidate());
						return super.compositesByMeta(holder);
					}

					@Override
					protected Stream<T> compositesBySuper(T holder) {
						ObservableList<T> ol = localBase.getObservableComposites();
						ObservableList<T> filtered = ol.filtered(x -> x.getSupers().contains(holder));
						invalidators.add(filtered);
						bind(filtered);
						return super.compositesBySuper(holder);
					}
				};

			};
		}.inheritanceStream().collect(Collectors.toList());
		// invalidators.forEach(ObservableInheritanceComputer2.this::bind);
		return FXCollections.observableList(internal);
	}

}
