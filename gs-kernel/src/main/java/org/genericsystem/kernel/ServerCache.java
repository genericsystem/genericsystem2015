package org.genericsystem.kernel;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.ITransaction;

public class ServerCache extends AbstractCache<Generic> {

	protected ServerCache(ServerEngine engine) {
		this(engine, new ContextEventListener<Generic>() {
		});
	}

	protected ServerCache(ServerEngine engine, ContextEventListener<Generic> listener) {
		super(engine, listener);
	}

	@Override
	protected ITransaction<Generic> buildTransaction(AbstractRoot<Generic> root) {
		return new Transaction((Root) root);
	}

}
