package org.genericsystem.distributed.cacheonserver.ui.table;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;

public class TitleRowModel extends GenericModel {

	private final ObservableList<TitleCellModel> titleCellModels;

	public TitleRowModel(Generic generic, ObservableList<Generic> attributes, Function<Generic, TitleCellModel> subModelBuilder) {
		super(generic);
		titleCellModels = new Transformation2<>(attributes, subModelBuilder);
	}

	public ObservableList<TitleCellModel> getTitleCellModels() {
		return titleCellModels;
	}

	public ObservableValue<String> getFirstCellString() {
		Serializable value = getGeneric().getValue();
		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)");
	}

}
