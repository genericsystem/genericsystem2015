package org.genericsystem.todomvc;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;

public class Todo {

	private ObservableValue<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	Todo(String text) {
		todoString = new ReadOnlyObjectWrapper<>(text);
	}

	/*********************************************************************************************************************************/

	public ObservableValue<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}
}
