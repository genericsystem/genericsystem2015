package org.genericsystem.todoApp;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;

public class Todo {
	public ObjectProperty<String> stringProperty = new SimpleObjectProperty<>();

	public Todo(ObjectProperty<String> stringProperty) {
		this.stringProperty = stringProperty;
	}

	public Todo(String string) {
		this.stringProperty.set(string);
	}
}
