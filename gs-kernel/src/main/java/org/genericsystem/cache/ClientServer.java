package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Server;

public class ClientServer implements Server {
	private final Server root;

	public ClientServer(Server root) {
		this.root = root;
	}

	@Override
	public Vertex getVertex(long id) {
		return root.getVertex(id);
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		return root.getDependencies(ts, id);
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		root.apply(ts, removes, adds);
	}

	@Override
	public long pickNewTs() {
		return root.pickNewTs();
	}

	@Override
	public void close() {
		root.close();
	}
}
