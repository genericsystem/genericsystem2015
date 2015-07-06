package org.genericsystem.cache;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;

public class ClientCache extends AbstractCache<ClientGeneric> {

	@Override
	public ClientEngine getRoot() {
		return (ClientEngine) super.getRoot();
	};

	protected ClientCache(ClientEngine root) {
		this(root, new ContextEventListener<ClientGeneric>() {
		});
	}

	protected ClientCache(ClientEngine root, ContextEventListener<ClientGeneric> listener) {
		super(root, listener);
	}

	@Override
	protected ClientTransaction buildTransaction(AbstractRoot<ClientGeneric> root) {
		return new ClientTransaction(((ClientEngine) root), root.pickNewTs());
	}
}
