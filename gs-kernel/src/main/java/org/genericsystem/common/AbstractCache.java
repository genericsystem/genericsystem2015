package org.genericsystem.common;

import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.CompletableFuture;

import javafx.beans.Observable;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultCache;

public abstract class AbstractCache extends CheckedContext implements DefaultCache<Generic> {

	protected AbstractCache(AbstractRoot root) {
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

	@Override
	public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic vertex) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Observable getInvalidator(Generic generic) {
		throw new UnsupportedOperationException();
	}

}
