package org.genericsystem.distributed.cacheonclient;

import org.genericsystem.common.Cache;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

public class ClientCache extends Cache {

	protected ClientCache(ClientEngine root) {
		super(root);
	}

	public ClientCache(ClientEngine root, ContextEventListener<Generic> listener) {
		super(root, listener);
	}

	@Override
	public ClientEngine getRoot() {
		return (ClientEngine) super.getRoot();
	}

	@Override
	protected IDifferential<Generic> buildTransaction() {
		ClientEngine root = getRoot();
		return new FrontEndTransaction(root, root.pickNewTs());
	}

}
