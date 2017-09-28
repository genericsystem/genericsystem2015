package org.genericsystem.common;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.defaults.DefaultGeneric;

import io.reactivex.Observable;

/**
 * @author Nicolas Feybesse
 *
 */
public interface IDifferential<T extends DefaultGeneric<T>> {

	long getTs();

	Snapshot<T> getDependencies(T generic);

	Observable<?> getDifferentialObservable();

	void apply(Snapshot<T> removes, Snapshot<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	Observable<Generic> getAddsObservable(Generic generic);

	Observable<Generic> getRemovesObservable(Generic generic);

	default AbstractCache getCache() {
		return null;
	}

	// CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);

}
