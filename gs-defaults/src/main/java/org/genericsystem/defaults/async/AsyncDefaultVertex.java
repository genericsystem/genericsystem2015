package org.genericsystem.defaults.async;

import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultVertex;

public interface AsyncDefaultVertex<T extends AsyncDefaultVertex<T>> extends DefaultVertex<T> {

	CompletableFuture<Snapshot<T>> getAsyncInstances();

}
