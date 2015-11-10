package org.genericsystem.distributed.cacheonclient;

import java.util.List;

import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

/**
 * @author Nicolas Feybesse
 *
 */
public interface AsyncIDifferential extends IDifferential<Generic> {
	public ObservableValue<List<Generic>> getDependenciesObservableList(Generic generic);

	public Wrappable<Generic> getWrappableDependencies(Generic generic);
}
