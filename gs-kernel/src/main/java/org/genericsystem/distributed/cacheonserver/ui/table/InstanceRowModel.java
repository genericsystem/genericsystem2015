package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class InstanceRowModel extends CompositeModel<CompositeModel<GenericModel>> {

	public InstanceRowModel(Conf conf) {
		super(conf);
	}

	public void remove() {
		getGeneric().remove();
	}
}
