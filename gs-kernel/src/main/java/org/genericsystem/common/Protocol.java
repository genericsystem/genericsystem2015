package org.genericsystem.common;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

/**
 * @author Nicolas Feybesse
 *
 */
public interface Protocol {

	long pickNewTs();

	Vertex getVertex(long id);

	Vertex[] getDependencies(long ts, long id);

	void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	void close();

}
