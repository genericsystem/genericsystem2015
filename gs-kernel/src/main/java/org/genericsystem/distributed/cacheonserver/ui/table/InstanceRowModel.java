package org.genericsystem.distributed.cacheonserver.ui.table;

import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class InstanceRowModel extends CompositeModel<CompositeModel<GenericModel>> {

	public InstanceRowModel(Generic[] generics, StringExtractor stringExtractor, ObservableList<CompositeModel<GenericModel>> subModels) {
		super(generics, stringExtractor, subModels);
	}

	public void remove() {
		getGeneric().remove();
	}
}
