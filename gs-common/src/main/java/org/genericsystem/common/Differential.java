package org.genericsystem.common;

import java.util.stream.Stream;

import org.genericsystem.api.core.Filters;
import org.genericsystem.api.core.IGeneric;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.tools.BindingsTools;

import javafx.beans.Observable;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;

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
			public Stream<Generic> stream() {
				return Stream.concat(adds.contains(generic) ? Stream.empty() : subDifferential.getDependencies(generic).stream().filter(x -> !removes.contains(x)), adds.stream().filter(x -> generic.isDirectAncestorOf(x)));
			}

			@Override
			public <U extends IGeneric<U>> Snapshot<Generic> filter(Filters filter, U vertex) {
				return new Snapshot<Generic>() {

					@Override
					public Stream<Generic> stream() {
						return Stream.concat(adds.contains(generic) ? Stream.empty() : subDifferential.getDependencies(generic).filter(filter, vertex).stream().filter(x -> !removes.contains(x)),
								adds.stream().filter(x -> generic.isDirectAncestorOf(x) && filter.getFilter(vertex).test(x)));
					}

				};
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
