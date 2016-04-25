package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class InstanceRowModel extends GenericCompositeModel<CompositeModel<GenericModel>> {

	public InstanceRowModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, CompositeModel<GenericModel>> elementBuilder) {
		super(generic, observableListExtractor, elementBuilder);
	}

	public void remove() {
		getGeneric().remove();
	}
}
