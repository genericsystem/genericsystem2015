package org.genericsystem.kernel;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

public class Cache extends AbstractCache {

	public Cache(Engine root) {
		super(root);
	}

	public Cache(Engine engine, ContextEventListener<Generic> listener) {
		super(engine, listener);
	}

	@Override
	public AbstractServer getRoot() {
		return (AbstractServer) super.getRoot();
	}

	@Override
	protected IDifferential<Generic> buildTransaction() {
		return new Transaction(getRoot());
	}

}