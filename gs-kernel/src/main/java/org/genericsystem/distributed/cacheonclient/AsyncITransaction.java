package org.genericsystem.distributed.cacheonclient;

import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

public interface AsyncITransaction extends IDifferential<Generic> {
	public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);

	public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic);
}
