package org.genericsystem.todomvc;

import java.util.Objects;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.todomvc.Todos.Completed;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

/**
 * @author Nicolas Feybesse
 *
 */
public class Todo extends Model {

	private final Generic generic;
	private Property<String> todoString;
	private ObservableValue<Boolean> completed;// = new SimpleBooleanProperty();
	private ObservableValue<String> completionString;

	Todo(TodoList parentModel, Generic generic) {
		this.generic = generic;
		todoString = new ReadOnlyObjectWrapper<>(Objects.toString(generic));
		ObservableValue<Generic> observableHolder = generic.getObservableHolder(generic.getRoot().find(Completed.class));
		completed = Bindings.createBooleanBinding(() -> observableHolder.getValue() != null ? (Boolean) observableHolder.getValue().getValue() : false, observableHolder);
		completionString = Bindings.createStringBinding(() -> Boolean.TRUE.equals(completed.getValue()) ? "completed" : null, completed);
	}

	/*********************************************************************************************************************************/

	public Property<String> getTodoString() {
		return todoString;
	}

	public ObservableValue<Boolean> getCompleted() {
		return completed;
	}

	public void setCompletion(boolean completion) {
		generic.setHolder(generic.getRoot().find(Completed.class), completion);
	}

	public void remove() {
		generic.remove();
	}
}
