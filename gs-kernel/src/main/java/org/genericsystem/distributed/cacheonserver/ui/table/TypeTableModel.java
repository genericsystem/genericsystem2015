package org.genericsystem.distributed.cacheonserver.ui.table;

import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;

/**
 * @author Nicolas Feybesse
 *
 */

public class TypeTableModel extends CompositeModel<InstanceRowModel> {

	public TypeTableModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
	}

	public TypeTableModel(Generic[] generics, StringExtractor stringExtractor, ObservableList<InstanceRowModel> subModels) {
		super(generics, stringExtractor, subModels);
	}

}
