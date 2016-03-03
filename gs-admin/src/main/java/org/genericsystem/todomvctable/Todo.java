package org.genericsystem.todomvctable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.distributed.ui.Model;

public class Todo extends Model {

	Property<String> stringProperty = new SimpleStringProperty();

	public ObservableValue<String> getStringProperty() {
		return stringProperty;
	}
}