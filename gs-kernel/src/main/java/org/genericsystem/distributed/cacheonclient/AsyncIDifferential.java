package org.genericsystem.distributed.cacheonclient;

import java.util.concurrent.CompletableFuture;

import javafx.beans.Observable;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

/**
 * @author Nicolas Feybesse
 *
 */
public interface AsyncIDifferential extends IDifferential<Generic> {
	// public ObservableValue<CompletableFuture<Snapshot<Generic>>> getObervableDependenciesPromise(Generic generic);

	// public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic);

	public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic);

	public Observable getInvalidator(Generic generic);
}
