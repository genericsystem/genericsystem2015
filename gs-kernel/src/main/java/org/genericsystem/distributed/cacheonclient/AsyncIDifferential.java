package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

/**
 * @author Nicolas Feybesse
 *
 */
public interface AsyncIDifferential extends IDifferential<Generic> {
	public ObservableList<Generic> getDependenciesObservableList(Generic generic);
}
