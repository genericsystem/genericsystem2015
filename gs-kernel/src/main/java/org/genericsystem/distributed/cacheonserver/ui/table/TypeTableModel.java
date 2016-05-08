package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends CompositeModel<InstanceRowModel> {

	public TypeTableModel(Generic[] generics, StringExtractor stringExtractor) {
		super(generics, stringExtractor);
	}
}
