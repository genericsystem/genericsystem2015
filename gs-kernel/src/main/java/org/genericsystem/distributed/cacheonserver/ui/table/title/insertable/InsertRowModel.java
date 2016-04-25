package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeModel;

public class InsertRowModel extends CompositeModel<InsertAttributeCellModel> {

	private final Property<String> inputString = new SimpleStringProperty();
	private final Generic generic;

	public InsertRowModel(Generic generic, Function<Generic, ObservableList<Generic>> observableListExtractor, Function<Generic, InsertAttributeCellModel> elementBuilder) {
		super(generic, observableListExtractor, elementBuilder);
		this.generic = generic;
	}

	public Property<String> getInputString() {
		return inputString;
	}

	public void create() {
		Generic instance = generic.setInstance(inputString.getValue());
		for (InsertAttributeCellModel model : getSubModels())
			instance.setHolder(model.getGeneric(), model.getInputString().getValue());
	}

}
