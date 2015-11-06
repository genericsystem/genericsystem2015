package org.genericsystem.common;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;

public class Differential implements IDifferential<Generic> {

	private Map<Generic, ObservableList<Generic>> mapCache = new HashMap<Generic, ObservableList<Generic>>() {

		@Override
		public javafx.collections.ObservableList<Generic> get(Object key) {

			ObservableList<Generic> result = super.get(key);
			if (result != null) {
				return result;
			}

			result = new ListBinding<Generic>() {
				{
					super.bind(addsObservable, removesObservable, differential.getObservableDependencies((Generic) key));
				}

				@Override
				public void dispose() {
					super.unbind(addsObservable, removesObservable, differential.getObservableDependencies((Generic) key));
				}

				@Override
				protected ObservableList<Generic> computeValue() {
					return FXCollections.concat(addsObservable.contains(key) ? FXCollections.emptyObservableList() : differential.getObservableDependencies((Generic) key).filtered(x -> !removesObservable.contains(x)),
							addsObservable.filtered(x -> ((Generic) key).isDirectAncestorOf(x)));

				}

				@Override
				public String toString() {
					if (!isValid())
						get();
					return super.toString();
				}

			};

			super.put((Generic) key, result);
			return result;
		};

	};

	private ObservableList<Generic> addsObservable;
	private ObservableList<Generic> removesObservable;

	private final IDifferential<Generic> differential;
	private final PseudoConcurrentCollection<Generic> adds = new PseudoConcurrentCollection<>();
	private final PseudoConcurrentCollection<Generic> removes = new PseudoConcurrentCollection<>();

	@Override
	public ObservableList<Generic> getObservableDependencies(Generic generic) {
		return mapCache.get(generic);
	}

	// private ObservableList<Generic> buildObservableList(Generic generic) {
	// return FXCollections.concat(addsObservable.contains(generic) ? FXCollections.emptyObservableList() : differential.getDependencies(generic).toObservable().filtered(x -> !removesObservable.contains(x)),
	// addsObservable.filtered(x -> generic.isDirectAncestorOf(x)));
	// }

	public Differential(IDifferential<Generic> subCache) {
		this.differential = subCache;
		addsObservable = FXCollections.observableArrayList(); // constructor with list parameter builds observable alongside
		removesObservable = FXCollections.observableArrayList();

	}

	// public Stream<Generic> getLivingToRespawn() {
	// return adds.stream().filter(g -> {
	// return g.getTs() == Long.MAX_VALUE && g.isAlive();
	// });
	// }

	public IDifferential<Generic> getSubCache() {

		return differential;
	}

	public int getCacheLevel() {
		return differential instanceof Differential ? ((Differential) differential).getCacheLevel() + 1 : 0;
	}

	void checkConstraints(Checker checker) throws RollbackException {
		adds.forEach(x -> checker.checkAfterBuild(true, true, x));
		removes.forEach(x -> checker.checkAfterBuild(false, true, x));
	}

	protected Generic plug(Generic generic) {
		adds.add(generic);
		addsObservable.add(generic);
		// mapCache.get(generic);
		return generic;
	}

	protected void unplug(Generic generic) {

		if (!adds.remove(generic)) {
			removesObservable.add(generic);
			removes.add(generic);
		} else
			addsObservable.remove(generic);
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {

		return new Snapshot<Generic>() {
			@Override
			public Generic get(Object o) {
				Generic result = adds.get(o);
				if (result != null)
					return generic.isDirectAncestorOf(result) ? result : null;
				return !removes.contains(o) ? differential.getDependencies(generic).get(o) : null;
			}

			@Override
			public Stream<Generic> stream() {
				return Stream.concat(adds.contains(generic) ? Stream.empty() : differential.getDependencies(generic).stream().filter(x -> !removes.contains(x)), adds.stream().filter(x -> generic.isDirectAncestorOf(x)));
			}
		};
	}

	void apply() throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		getSubCache().apply(removes, adds);
	}

	@Override
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		for (Generic generic : removes)
			unplug(generic);
		for (Generic generic : adds)
			plug(generic);
	}

	@Override
	public long getTs() {
		return getSubCache().getTs();
	}
}