package org.genericsystem.todomvctable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

public class TodoColumn {
	Property<String> title = new SimpleObjectProperty<String>();

	public ObservableValue<String> getTitle() {
		return title;
	}
}