package org.genericsystem.distributed.cacheonserver.todomvc;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class Todo extends Model {

	private final Generic generic;
	private Property<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	Todo(TodoList parentModel, Generic generic) {
		this.generic = generic;
		todoString = new ReadOnlyObjectWrapper<>(Objects.toString(generic));
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
		generic.remove();
	}
}
