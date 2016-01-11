package org.genericsystem.distributed.cacheonclient;

import javafx.beans.Observable;

import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

/**
 * @author Nicolas Feybesse
 *
 */
public interface AsyncIDifferential extends IDifferential<Generic> {
	public Observable getInvalidator(Generic generic);
}
