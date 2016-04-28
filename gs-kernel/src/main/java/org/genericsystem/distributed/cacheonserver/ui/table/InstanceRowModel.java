package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.models.GenericCompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class InstanceRowModel extends GenericCompositeModel<GenericCompositeModel<GenericModel>> {

	// public InstanceRowModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, CompositeModel<GenericModel>> elementBuilder) {
	// this(new CompositeConf<>(generic, observableListExtractor, elementBuilder));
	// }

	public InstanceRowModel(CompositeConf<GenericCompositeModel<GenericModel>> conf) {

		super(conf);
	}

	public void remove() {
		getGeneric().remove();
	}
}
