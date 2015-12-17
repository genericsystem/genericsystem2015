package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;

public class HolderWrapper {
	Generic holder;
	private StringProperty stringProperty = new SimpleStringProperty();

	public HolderWrapper(Generic hold) {
		this.holder = hold;
		stringProperty.set(hold.getValue().toString());
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}
}
