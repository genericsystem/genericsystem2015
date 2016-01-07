package org.genericsystem.todomvc;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSCheckBox;
import org.genericsystem.ui.components.GSLabel;

public class Todo {

	private ObservableValue<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	Todo(String text) {
		todoString = new ReadOnlyObjectWrapper<>(text);
	}

	/*********************************************************************************************************************************/

	public static void init(Element<HBox> todoHBox) {
		new GSCheckBox(todoHBox, Todo::getCompleted);
		new GSLabel(todoHBox, Todo::getTodoString).setPrefWidth(141).setOptionalStyleClass(Todo::getCompleted, "completed");
		new GSButton(todoHBox, "select").setMetaAction((todoList, todo) -> ((TodoList) todoList).getSelection().setValue((Todo) todo)).setPrefWidth(90);
		new GSButton(todoHBox, "remove").setMetaAction((todoList, todo) -> ((TodoList) todoList).getTodos().remove(todo)).setPrefWidth(90);
	}

	/*********************************************************************************************************************************/

	public ObservableValue<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}
}
