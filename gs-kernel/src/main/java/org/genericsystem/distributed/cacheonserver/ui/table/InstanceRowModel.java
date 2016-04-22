package org.genericsystem.distributed.cacheonserver.ui.table;

import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeGenericModel;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class InstanceRowModel extends CompositeGenericModel<CompositeModel<GenericModel>> {

	public InstanceRowModel(Generic instance, ObservableList<CompositeModel<GenericModel>> instanceAttributesModels) {
		super(instance, instanceAttributesModels);
	}

	public void remove() {
		getGeneric().remove();
	}
}
