package org.genericsystem.distributed.cacheonserver.ui.table.title;

import java.io.Serializable;
import java.util.Objects;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TitleRowModel extends CompositeModel<GenericModel> {

	public TitleRowModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
	}

	public ObservableValue<String> getFirstCellString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)");
	}

}
