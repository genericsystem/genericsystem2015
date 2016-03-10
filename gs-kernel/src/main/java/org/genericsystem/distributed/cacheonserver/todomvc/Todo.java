package org.genericsystem.distributed.cacheonserver.todomvc;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import org.genericsystem.distributed.ui.Model;

public class Todo extends Model {

	private Property<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	Todo(TodoList parentModel, String text) {
		todoString = new ReadOnlyObjectWrapper<>(text);
	}

	/*********************************************************************************************************************************/

	public Property<String> getTodoString() {
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
