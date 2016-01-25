package org.genericsystem.todomvctable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import org.genericsystem.ui.Model;

public class TodoColumn extends Model {
	public TodoColumn(Model parent) {
		super(parent);
	}

	Property<String> title = new SimpleObjectProperty<String>();

	public ObservableValue<String> getTitle() {
		return title;
	}
}