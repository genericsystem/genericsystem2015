package org.genericsystem.cache;

import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.kernel.Checker;

public class Differential implements IDifferential {

	private final IDifferential differential;
	private final PseudoConcurrentCollection<Generic> adds = new PseudoConcurrentCollection<>();
	private final PseudoConcurrentCollection<Generic> removes = new PseudoConcurrentCollection<>();

	public Differential(IDifferential subCache) {
		this.differential = subCache;
	}

	public IDifferential getSubCache() {
		return differential;
	}

	public int getCacheLevel() {
		return differential instanceof Differential ? ((Differential) differential).getCacheLevel() + 1 : 0;
	}

	// @Override
	// public boolean isAlive(Generic generic) {
	// return adds.contains(generic) || (!removes.contains(generic) && differential.isAlive(generic));
	// }

	void checkConstraints(Checker checker) throws RollbackException {
		adds.forEach(x -> checker.checkAfterBuild(true, true, x));
		removes.forEach(x -> checker.checkAfterBuild(false, true, x));
	}

	protected Generic plug(Generic generic) {
		adds.add(generic);
		return generic;
	}

	protected void unplug(Generic generic) {
		if (!adds.remove(generic))
			removes.add(generic);
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
}
