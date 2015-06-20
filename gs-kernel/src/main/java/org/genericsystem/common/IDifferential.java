package org.genericsystem.common;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.defaults.DefaultVertex;

/**
 * @author Nicolas Feybesse
 *
 */
public interface IDifferential<T extends DefaultVertex<T>> {

	Snapshot<T> getDependencies(T generic);

	void apply(Iterable<T> removes, Iterable<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

}
