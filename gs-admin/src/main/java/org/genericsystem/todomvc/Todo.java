package org.genericsystem.todomvc;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSCheckBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.utils.OneShotBindings;

public class Todo {

	private Property<TodoList> parentProperty = new SimpleObjectProperty<TodoList>();
	private ObservableValue<String> todoString = OneShotBindings.createInitializer(parentProperty, todolist -> todolist.getName().getValue());
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	/*********************************************************************************************************************************/

	public static void init(Element<HBox> todoHBox) {
		new GSCheckBox(todoHBox, Todo::getCompleted);
		new GSLabel(todoHBox, Todo::getTodoString).setPrefWidth(141).setOptionalStyleClass(Todo::getCompleted, "completed");
		new GSButton(todoHBox, "select").setMetaAction(TodoList::select).setPrefWidth(90);
		new GSButton(todoHBox, "remove").setMetaAction(TodoList::remove).setPrefWidth(90).setMetaAction(TodoList::remove);
	}

	/*********************************************************************************************************************************/

	public Property<TodoList> getParentProperty() {
		return parentProperty;
	}

	public ObservableValue<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}
}
