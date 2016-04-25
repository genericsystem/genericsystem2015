package org.genericsystem.distributed.cacheonserver.ui.table.title;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TitleRowModel extends CompositeModel<GenericModel> {

	private final Generic generic;

	public TitleRowModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, GenericModel> elementBuilder) {
		super(generic, observableListExtractor, elementBuilder);
		this.generic = generic;
	}

	public ObservableValue<String> getFirstCellString() {
		Serializable value = generic.getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)");
	}

}
