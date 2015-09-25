package org.genericsystem.common;

import java.util.Arrays;
import java.util.Collections;

import org.genericsystem.defaults.DefaultCache;

public abstract class AbstractCache extends CheckedContext implements DefaultCache<Generic> {

	protected AbstractCache(AbstractEngine root) {
		super(root);
	}

	public Generic setMeta(int dim) {
		return setInstance(null, Collections.emptyList(), getRoot().getValue(), Arrays.asList(rootComponents(dim)));
	}

	@SuppressWarnings("unchecked")
	public final <U extends AbstractCache> U start() {
		return (U) getRoot().start(this);
	}

	public final void stop() {
		getRoot().stop(this);
	}

}
