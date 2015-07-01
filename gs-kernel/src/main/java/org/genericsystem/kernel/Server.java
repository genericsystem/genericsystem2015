package org.genericsystem.kernel;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.defaults.DefaultRoot;

public interface Server extends DefaultRoot<Generic> {

	final static long[] EMPTY = new long[] {};

	default Vertex getVertex(long id) {
		return ((Root) this).getGenericById(id).getVertex();
	}

	default long[] getDependencies(long ts, long id) {
		Generic genericById = ((Root) this).getGenericById(id);
		return genericById != null ? genericById.getProxyHandler().getDependencies().stream(ts).mapToLong(generic -> generic.getTs()).toArray() : EMPTY;
	}

	default void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		new Transaction((Root) this, ts).remoteApply(removes, adds);
	}
}
