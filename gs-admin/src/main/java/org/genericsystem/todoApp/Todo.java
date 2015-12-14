package org.genericsystem.todoApp;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.bindings.OneShotBindings;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSLabel;

public class Todo {

	Property<TodoList> parentProperty = new SimpleObjectProperty<TodoList>();
	ObservableValue<String> todoString = OneShotBindings.createInitializer(parentProperty, todolist -> todolist.getName().getValue());
	Property<Boolean> completed = new SimpleBooleanProperty(false);

	public Property<TodoList> getParentProperty() {
		return parentProperty;
	}

	public ObservableValue<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}

	public void select() {
		parentProperty.getValue().selection.setValue(this);
	}

	public void remove() {
		parentProperty.getValue().todos.remove(this);
	}

	public static void init(Element<HBox> todoHBox) {
		Element<CheckBox> todoCheckBox = new Element<>(todoHBox, CheckBox.class);
		todoCheckBox.addBidirectionalBinding(CheckBox::selectedProperty, Todo::getCompleted);
		GSLabel todoLabel = new GSLabel(todoHBox, Todo::getTodoString).setPrefWidth(141).setOptionalStyleClass(Todo::getCompleted, "completed");
		GSButton todoSelectButton = new GSButton(todoHBox, "select", Todo::select).setPrefWidth(90);
		GSButton todoRemoveButton = new GSButton(todoHBox, "remove", Todo::remove).setPrefWidth(90);
	}
}
