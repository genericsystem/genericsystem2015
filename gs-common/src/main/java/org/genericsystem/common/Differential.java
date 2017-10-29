package org.genericsystem.common;

import io.reactivex.Observable;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;

import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.api.tools.Memoizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Nicolas Feybesse
 *
 */
public class Differential implements IDifferential<Generic> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private final IDifferential<Generic> subDifferential;
	protected final PseudoConcurrentCollection<Generic> adds = new PseudoConcurrentCollection<>();
	protected final PseudoConcurrentCollection<Generic> removes = new PseudoConcurrentCollection<>();
	private Function<Generic, Observable<Generic>> addsM = Memoizer.memoize(generic -> Observable.merge(getSubDifferential().getAdds(generic), adds.getFilteredAdds(generic::isDirectAncestorOf), removes.getFilteredRemoves(generic::isDirectAncestorOf))
			.share());
	private Function<Generic, Observable<Generic>> remsM = Memoizer.memoize(generic -> Observable.merge(getSubDifferential().getRemovals(generic), removes.getFilteredAdds(generic::isDirectAncestorOf), adds.getFilteredRemoves(generic::isDirectAncestorOf))
			.share());

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

	@Override
	public AbstractCache getCache() {
		return subDifferential.getCache();
	}

	public int getCacheLevel() {
		return subDifferential instanceof Differential ? ((Differential) subDifferential).getCacheLevel() + 1 : 0;
	}

	public IntegerProperty getCacheLevelObservable() {
		return subDifferential instanceof Differential ? new SimpleIntegerProperty(((Differential) subDifferential).getCacheLevel() + 1) : new SimpleIntegerProperty(0);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Observable<Differential> getDifferentialObservable() {
		return (Observable<Differential>) subDifferential.getDifferentialObservable();
	}

	void checkConstraints(Checker checker) throws RollbackException {
		adds.forEach(x -> checker.checkAfterBuild(true, true, x));
		removes.forEach(x -> checker.checkAfterBuild(false, true, x));
	}

	protected Generic plug(Generic generic) {
		// assert generic.getOtherTs()[0] == Long.MAX_VALUE;
		adds.add(generic);
		return generic;
	}

	protected void unplug(Generic generic) {
		if (!adds.remove(generic))
			removes.add(generic);
	}

	private final Function<Generic, Snapshot<Generic>> getDepsM = Memoizer.<Generic, Snapshot<Generic>> memoize(generic -> new Snapshot<Generic>() {
		private Observable<Generic> addsObs = getDifferentialObservable().switchMap(diff -> diff.getAdds(generic).filter(g -> getCache().isAlive(g))).share();
		private Observable<Generic> removalsObs = getDifferentialObservable().switchMap(diff -> diff.getRemovals(generic).filter(g -> !getCache().isAlive(g))).share();

		@Override
		public Generic get(Object o) {
			Generic result = adds.get(o);
			if (result != null)
				return generic.isDirectAncestorOf(result) ? result : null;
			return !removes.contains(o) ? subDifferential.getDependencies(generic).get(o) : null;
		}

		@Override
		public Stream<Generic> unfilteredStream() {
			return Stream.concat(adds.contains(generic) ? Stream.empty() : subDifferential.getDependencies(generic).filter(new IndexFilter(FiltersBuilder.NOT_CONTAINED_IN_PARAM, removes.toSet())).stream(),
					adds.filter(new IndexFilter(FiltersBuilder.IS_DIRECT_DEPENDENCY_OF, generic)).stream());
		}

		@Override
		public Snapshot<Generic> filter(List<IndexFilter> filters) {
			List<IndexFilter> filters_ = new ArrayList<>(filters);
			filters_.add(new IndexFilter(FiltersBuilder.NOT_CONTAINED_IN_PARAM, removes.toSet()));
			return Snapshot.super.filter(filters_);
		}

		@Override
		public Observable<Generic> getAdds() {
			return addsObs;
		}

		@Override
		public Observable<Generic> getRemovals() {
			return removalsObs;
		}
	});

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		return getDepsM.apply(generic);
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

	public void unApply() {
		adds.forEach(add -> adds.remove(add));
		removes.forEach(rem -> removes.remove(rem));
	}

	@Override
	public long getTs() {
		return getSubDifferential().getTs();
	}

	@Override
	public Observable<Generic> getAdds(Generic generic) {
		return addsM.apply(generic);
	}

	@Override
	public Observable<Generic> getRemovals(Generic generic) {
		return remsM.apply(generic);
	}
}
