package org.genericsystem.distributed.cacheonserver.ui.table;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class GenericModel extends Model {

	private final Generic generic;
	private final Property<String> string;

	public GenericModel(Generic generic) {
		this.generic = generic;
		this.string = new ReadOnlyStringWrapper(Objects.toString(generic.getValue()));
	}

	protected Generic getGeneric() {
		return generic;
	}

	public ObservableValue<String> getString() {
		return string;
	}
}
