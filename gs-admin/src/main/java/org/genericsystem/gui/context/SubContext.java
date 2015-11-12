package org.genericsystem.gui.context;

import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;

public class SubContext extends AbstractContext {

	public ObservableValue<Generic> observableGeneric;

	public SubContext(IContext parent) {
		super(parent);
	}
}
