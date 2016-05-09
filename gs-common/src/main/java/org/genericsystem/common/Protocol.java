package org.genericsystem.common;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

/**
 * @author Nicolas Feybesse
 *
 */
public interface Protocol {

	public static final int PICK_NEW_TS = 0;
	public static final int GET_DEPENDENCIES = 1;
	public static final int GET_VERTEX = 2;
	public static final int APPLY = 3;
	public static final int SHIFT_TS = 4;

	long pickNewTs();

	Vertex getVertex(long id);

	Vertex[] getDependencies(long ts, long id);

	void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException;

	void close();

}
