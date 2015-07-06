package org.genericsystem.kernel;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;

public interface Server {

	final static long[] EMPTY = new long[] {};

	Vertex getVertex(long id);

	long[] getDependencies(long ts, long id);

	void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	long pickNewTs();

	void close();
}
