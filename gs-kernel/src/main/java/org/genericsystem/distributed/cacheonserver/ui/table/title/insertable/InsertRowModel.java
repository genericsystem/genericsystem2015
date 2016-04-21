package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.CompositeGenericModel;

public class InsertRowModel extends CompositeGenericModel<InsertAttributeCellModel> {

	private final Property<String> inputString = new SimpleStringProperty();

	public InsertRowModel(Generic type, ObservableList<InsertAttributeCellModel> attributeCellModels) {
		super(type, attributeCellModels);
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
