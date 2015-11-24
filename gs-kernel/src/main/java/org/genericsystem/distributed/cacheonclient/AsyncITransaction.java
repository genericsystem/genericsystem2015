package org.genericsystem.distributed.cacheonclient;

import java.util.concurrent.CompletableFuture;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

public interface AsyncITransaction extends IDifferential<Generic> {
	public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);
}
