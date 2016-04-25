package org.genericsystem.distributed.cacheonserver.ui.table.title.insertable;

import java.util.function.Function;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.models.GenericModel;

public class InsertAttributeCellModel extends GenericModel {

	private Property<String> inputString = new SimpleStringProperty();

	public InsertAttributeCellModel(Generic attribute, Function<Generic, String> extractor) {
		super(attribute, extractor);
	}

	public Property<String> getInputString() {
		return inputString;
	}
}
