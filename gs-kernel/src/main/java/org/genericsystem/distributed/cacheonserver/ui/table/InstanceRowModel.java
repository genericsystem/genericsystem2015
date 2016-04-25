package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class InstanceRowModel extends GenericCompositeModel<CompositeModel<GenericModel>> {

	// public InstanceRowModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, CompositeModel<GenericModel>> elementBuilder) {
	// this(new CompositeConf<>(generic, observableListExtractor, elementBuilder));
	// }

	public InstanceRowModel(CompositeConf<CompositeModel<GenericModel>> conf) {
		super(conf);
	}

	public void remove() {
		getGeneric().remove();
	}
}
