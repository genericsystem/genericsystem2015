package org.genericsystem.gui.context;

import javafx.beans.property.ObjectProperty;

import org.genericsystem.common.Generic;

public class GenericContext extends AbstractContext {

	public ObjectProperty<Generic> genericProperty;

	public GenericContext(IContext parent, ObjectProperty<Generic> genericProperty) {
		super(parent);
		this.genericProperty = genericProperty;
	}
}
