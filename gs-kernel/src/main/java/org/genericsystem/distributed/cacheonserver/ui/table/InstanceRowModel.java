package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class InstanceRowModel extends CompositeModel<CompositeModel<GenericModel>> {

	public InstanceRowModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
	}

	public void remove() {
		getGeneric().remove();
	}
}
