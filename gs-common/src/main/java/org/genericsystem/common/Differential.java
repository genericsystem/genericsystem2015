package org.genericsystem.common;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.tools.BindingsTools;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public class Differential implements IDifferential<Generic> {

	private final IDifferential<Generic> subDifferential;
	protected final PseudoConcurrentCollection<Generic> adds = new PseudoConcurrentCollection<>();
	protected final PseudoConcurrentCollection<Generic> removes = new PseudoConcurrentCollection<>();

	public Differential(IDifferential<Generic> subDifferential) {
		this.subDifferential = subDifferential;
	}

	public Stream<Generic> getLivingToRespawn() {
		return adds.stream().filter(g -> {
			return g.getTs() == Long.MAX_VALUE && g.isAlive();
		});
	}

	public PseudoConcurrentCollection<Generic> getAdds() {
		return adds;
	}

	public IDifferential<Generic> getSubDifferential() {
		return subDifferential;
	}

	public int getCacheLevel() {
		return subDifferential instanceof Differential ? ((Differential) subDifferential).getCacheLevel() + 1 : 0;
	}

	public IntegerProperty getCacheLevelObservable() {
		return subDifferential instanceof Differential ? new SimpleIntegerProperty(((Differential) subDifferential).getCacheLevel() + 1) : new SimpleIntegerProperty(0);
	}

	@Override
	public ObjectProperty<IDifferential<Generic>> getDifferentialProperty() {
		return subDifferential.getDifferentialProperty();
	}

	@Override
	public Map<Generic, ObservableList<Generic>> getDependenciesAsOservableListCacheMap() {
		return subDifferential.getDependenciesAsOservableListCacheMap();
	}

	void checkConstraints(Checker checker) throws RollbackException {
		adds.forEach(x -> checker.checkAfterBuild(true, true, x));
		removes.forEach(x -> checker.checkAfterBuild(false, true, x));
	}

	protected Generic plug(Generic generic) {
		// assert generic.getOtherTs()[0] == Long.MAX_VALUE;
		adds.add(generic);
		// System.out.println("Add : " + generic.info() + System.identityHashCode(generic));
		return generic;
	}

	protected void unplug(Generic generic) {
		if (!adds.remove(generic))
			removes.add(generic);
		// System.out.println("Remove : " + generic.info() + System.identityHashCode(generic));
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		return new Snapshot<Generic>() {
			@Override
			public Generic get(Object o) {
				Generic result = adds.get(o);
				if (result != null)
					return generic.isDirectAncestorOf(result) ? result : null;
				return !removes.contains(o) ? subDifferential.getDependencies(generic).get(o) : null;
			}

			@Override
			public Stream<Generic> unfilteredStream() {
				return Stream.concat(adds.contains(generic) ? Stream.empty() : subDifferential.getDependencies(generic).filter(new IndexFilter(FiltersBuilder.NOT_CONTAINED_IN_PARAM, new ArrayList<>(removes.toList()))).stream(),
						adds.filter(new IndexFilter(FiltersBuilder.IS_DIRECT_DEPENDENCY_OF, generic)).stream());
			}

			@Override
			public Snapshot<Generic> filter(List<IndexFilter> filters) {
				return new Snapshot<Generic>() {

					@Override
					public Stream<Generic> unfilteredStream() {
						List<IndexFilter> filters_ = new ArrayList<>(filters);
						filters_.add(new IndexFilter(FiltersBuilder.NOT_CONTAINED_IN_PARAM, new ArrayList<>(removes.toList())));
						return Stream.concat(adds.contains(generic) ? Stream.empty() : subDifferential.getDependencies(generic).filter(filters_).stream(), adds.filter(filters).stream().filter(x -> generic.isDirectAncestorOf(x)));
					}
				};
			}

			@Override
			public ObservableList<Generic> toObservableList() {
				ObservableList<Generic> result = getDependenciesAsOservableListCacheMap().get(generic);
				if (result == null) {
					@SuppressWarnings({ "unchecked", "rawtypes" })
					ObjectProperty<Differential> differentialProperty = (ObjectProperty) getDifferentialProperty();
					result = BindingsTools.createMinimalUnitaryChangesBinding(BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Generic>() {
						private final Observable invalidator = BindingsTools.createTransitive(differentialProperty, diff -> new Observable[] { diff.getObservable(generic) });
						{
							bind(invalidator);
							invalidate();
						}

						@Override
						protected ObservableList<Generic> computeValue() {
							return FXCollections.observableList(differentialProperty.getValue().getDependencies(generic).toList());
						}
					}));
					getDependenciesAsOservableListCacheMap().put(generic, result);
				}
				return result;
			}
		};
	}

	void apply() throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		getSubDifferential().apply(removes, adds);
	}

	@Override
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		this.removes.disableInvalidations();
		this.adds.disableInvalidations();
		for (Generic generic : removes)
			unplug(generic);
		for (Generic generic : adds)
			plug(generic);
		this.removes.enableInvalidations();
		this.adds.enableInvalidations();
	}

	@Override
	public long getTs() {
		return getSubDifferential().getTs();
	}

	@Override
	public final Observable getObservable(Generic generic) {
		return BindingsTools.create(getSubDifferential().getObservable(generic), adds.getFilteredInvalidator(generic, generic::isDirectAncestorOf), removes.getFilteredInvalidator(generic, generic::isDirectAncestorOf));
		// return ObservableBase.createObservable(getSubDifferential().getObservable(generic), adds.getFilteredInvalidator(generic, generic::isDirectAncestorOf), removes.getFilteredInvalidator(generic, generic::isDirectAncestorOf));
	}
}
