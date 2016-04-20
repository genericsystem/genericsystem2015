package org.genericsystem.distributed.cacheonserver.ui.table;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import org.genericsystem.common.Generic;

public class AttributeCellModel extends GenericModel {

	private Property<String> inputString = new SimpleStringProperty();

	public AttributeCellModel(Generic attribute) {
		super(attribute);
	}

	public Property<String> getInputString() {
		return inputString;
	}
}
