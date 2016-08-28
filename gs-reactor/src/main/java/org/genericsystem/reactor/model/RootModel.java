package org.genericsystem.reactor.model;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.Context;

/**
 * @author Nicolas Feybesse
 *
 */
public class RootModel extends Context {

	public RootModel(Root engine) {
		super(null, new Generic[] { engine });
	}
}
