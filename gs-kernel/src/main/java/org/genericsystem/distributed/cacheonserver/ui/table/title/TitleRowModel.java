package org.genericsystem.distributed.cacheonserver.ui.table.title;

import java.io.Serializable;
import java.util.Objects;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeGenericModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TitleRowModel extends CompositeGenericModel<GenericModel> {

	public TitleRowModel(Generic generic, ObservableList<GenericModel> titleCellModels) {
		super(generic, titleCellModels);
	}

	public ObservableValue<String> getFirstCellString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)");
	}

}
