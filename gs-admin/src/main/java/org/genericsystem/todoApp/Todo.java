package org.genericsystem.todoApp;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;

public class Todo {
	public ObjectProperty<String> stringProperty = new SimpleObjectProperty<String>();

	// ObservableList<String> obsString = FXCollections.observableArrayList();
	public Todo(ObjectProperty<String> stringProperty) {
		this.stringProperty = stringProperty;
	}

	public Todo(String string) {
		// TODO Auto-generated constructor stub
		this.stringProperty.set(string);
	}
}
