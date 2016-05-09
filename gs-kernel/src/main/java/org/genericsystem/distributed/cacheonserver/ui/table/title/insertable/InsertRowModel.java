package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.CompositeModel;

public class InsertRowModel extends CompositeModel<InsertAttributeCellModel> {

	private final Property<String> inputString = new SimpleStringProperty();

	public InsertRowModel(Generic[] generics, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, Builder<?> builder) {
		super(generics, stringExtractor, observableListExtractor, builder);
	}

	public Property<String> getInputString() {
		return inputString;
	}

	public void create() {
		Generic instance = getGeneric().setInstance(inputString.getValue());
		for (InsertAttributeCellModel model : getSubModels())
			instance.setHolder(model.getGeneric(), model.getInputString().getValue());
	}

}
