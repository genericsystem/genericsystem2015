package org.genericsystem.reactor.model;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;

/**
 * @author Nicolas Feybesse
 *
 */
public class RootModel extends GenericModel {

	public RootModel(Root engine) {
		super(null, new Generic[] { engine });
	}
}
