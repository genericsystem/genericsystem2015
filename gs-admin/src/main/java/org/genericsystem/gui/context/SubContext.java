package org.genericsystem.gui.context;

import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;

public class SubContext extends AbstractContext {

	public ObservableValue<Generic> observableGeneric;

	public SubContext(RootContext parent, int index) {
		super(parent);
		// observableGeneric = Bindings.valueAt(parent.observableGenericList, index);
	}
}
