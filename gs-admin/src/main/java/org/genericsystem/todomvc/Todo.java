package org.genericsystem.todomvc;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.todomvc.TodoList;

public class Todo extends Model {

	private ObservableValue<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	Todo(TodoList parentModel, String text) {
		todoString = new ReadOnlyObjectWrapper<>(text);
	}

	/*********************************************************************************************************************************/

	public ObservableValue<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}

	public void select() {
		((TodoList) getParent()).getSelection().setValue(this);
	}

	public void remove() {
		((TodoList) getParent()).getTodos().remove(this);
	}
}
