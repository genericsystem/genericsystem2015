package org.genericsystem.cache;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.kernel.Generic;

public class ClientCache extends AbstractCache<Generic> {

	@Override
	public ClientEngine getRoot() {
		return (ClientEngine) super.getRoot();
	};

	protected ClientCache(ClientEngine root) {
		this(root, new ContextEventListener<Generic>() {
		});
	}

	protected ClientCache(ClientEngine root, ContextEventListener<Generic> listener) {
		super(root, listener);
	}

	@Override
	protected ClientTransaction buildTransaction(AbstractRoot<Generic> root) {
		return new ClientTransaction(((ClientEngine) root), root.pickNewTs());
	}
}
