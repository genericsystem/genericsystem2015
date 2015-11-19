package org.genericsystem.distributed.cacheonclient;

import java.util.List;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

/**
 * @author Nicolas Feybesse
 *
 */
public interface AsyncIDifferential extends IDifferential<Generic> {
	public ObservableValue<List<Generic>> getDependenciesObservableList(Generic generic);

	public ObservableList<Generic> getWrappableDependencies(Generic generic);

	public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic);
}
