package org.genericsystem.cache;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;

public class ClientCache extends AbstractCache<Generic> {

	@Override
	public Engine getRoot() {
		return (Engine) super.getRoot();
	};

	protected ClientCache(Engine root) {
		this(root, new ContextEventListener<Generic>() {
		});
	}

	protected ClientCache(Engine root, ContextEventListener<Generic> listener) {
		super(root, listener);
	}

	@Override
	protected ClientTransaction buildTransaction(AbstractRoot<Generic> root) {
		return new ClientTransaction(((Engine) root), root.pickNewTs());
	}
}
