package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class InstanceRowModel extends CompositeModel<CompositeModel<?>> {

	public InstanceRowModel(Generic[] generics, StringExtractor stringExtractor) {
		super(generics, stringExtractor);
	}
}
