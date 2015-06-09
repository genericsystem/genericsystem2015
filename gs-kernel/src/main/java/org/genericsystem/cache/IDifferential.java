package org.genericsystem.cache;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

public interface IDifferential {

	// abstract boolean isAlive(Generic vertex);

	abstract Snapshot<Generic> getDependencies(Generic vertex);

	abstract void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

}
