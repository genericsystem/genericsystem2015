package org.genericsystem.todomvc;

import java.util.Objects;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag.GenericModelInterface;
import org.genericsystem.todomvc.Todos.Completed;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;

/**
 * @author Nicolas Feybesse
 *
 */
public class Todo extends Model implements GenericModelInterface {

	private final Generic generic;
	private Property<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty();

	Todo(TodoList parentModel, Generic generic) {
		this.generic = generic;
		todoString = new ReadOnlyObjectWrapper<>(Objects.toString(generic));
		ObservableValue<Generic> observableHolder = generic.getObservableHolder(generic.getRoot().find(Completed.class));
		if (observableHolder.getValue() != null && Boolean.TRUE.equals(observableHolder.getValue().getValue())) {
			completed.setValue(true);
		}
		// completed = Bindings.createBooleanBinding(() -> observableHolder.getValue() != null ? (Boolean) observableHolder.getValue().getValue() : false, observableHolder);
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

	public Generic getGeneric() {
		return generic;
	}
}
