package org.genericsystem.common;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

public interface ClientCacheProtocole {

	final static Vertex[] EMPTY = new Vertex[] {};

	Vertex getVertex(long id);

	Vertex[] getDependencies(long ts, long id);

	void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	long pickNewTs();

	void close();
}
