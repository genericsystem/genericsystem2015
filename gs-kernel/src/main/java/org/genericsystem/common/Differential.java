package org.genericsystem.common;

import java.util.stream.Stream;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.DefaultVertex;

public class Differential<T extends DefaultVertex<T>> implements IDifferential<T> {

	private final IDifferential<T> differential;
	private final PseudoConcurrentCollection<T> adds = new PseudoConcurrentCollection<>();
	private final PseudoConcurrentCollection<T> removes = new PseudoConcurrentCollection<>();

	public Differential(IDifferential<T> subCache) {
		this.differential = subCache;
	}

	public IDifferential<T> getSubCache() {
		return differential;
	}

	public int getCacheLevel() {
		return differential instanceof Differential ? ((Differential<T>) differential).getCacheLevel() + 1 : 0;
	}

	void checkConstraints(Checker<T> checker) throws RollbackException {
		adds.forEach(x -> checker.checkAfterBuild(true, true, x));
		removes.forEach(x -> checker.checkAfterBuild(false, true, x));
	}

	protected T plug(T generic) {
		// assert generic.getOtherTs()[0] == Long.MAX_VALUE;
		adds.add(generic);
		return generic;
	}

	protected void unplug(T generic) {
		if (!adds.remove(generic))
			removes.add(generic);
	}

	@Override
	public Snapshot<T> getDependencies(T generic) {
		return new Snapshot<T>() {
			@Override
			public T get(Object o) {
				T result = adds.get(o);
				if (result != null)
					return generic.isDirectAncestorOf(result) ? result : null;
				return !removes.contains(o) ? differential.getDependencies(generic).get(o) : null;
			}

			@Override
			public Stream<T> stream() {
				return Stream.concat(adds.contains(generic) ? Stream.empty() : differential.getDependencies(generic).stream().filter(x -> !removes.contains(x)), adds.stream().filter(x -> generic.isDirectAncestorOf(x)));
			}
		};
	}

	void apply() throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		getSubCache().apply(removes, adds);
	}

	@Override
	public void apply(Snapshot<T> removes, Snapshot<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		for (T generic : removes)
			unplug(generic);
		for (T generic : adds)
			plug(generic);
	}

	@Override
	public long getTs() {
		return getSubCache().getTs();
	}
}