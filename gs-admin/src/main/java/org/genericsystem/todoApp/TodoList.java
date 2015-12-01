package org.genericsystem.todoApp;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.genericsystem.todoApp.binding.Binding;

public class TodoList {

	private Property<String> name = new SimpleStringProperty();
	private ObservableList<Todo> todos = FXCollections.observableArrayList();
	private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Todo");
	private ObservableValue<Number> height = new SimpleDoubleProperty(200);

	public Property<String> getName() {
		return name;
	}

	public ObservableList<Todo> getTodos() {
		return todos;
	}

	public ObservableValue<String> getCreateButtonTextProperty() {
		return createButtonTextProperty;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public static class Todo {
		private Property<String> stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}
	}

	public Node init() {

		Element mainVBox = new Element(null, VBox.class, Binding.bindProperty(VBox::prefHeightProperty, TodoList::getHeight));
		Element todoCreateHBox = new Element(mainVBox, HBox.class);
		Element todosCreatLabel = new Element(todoCreateHBox, TextField.class, Binding.bindInputText(TextField::textProperty, TodoList::getName));
		Element todosCreateButton = new Element(todoCreateHBox, Button.class, Binding.bindProperty(Button::textProperty, TodoList::getCreateButtonTextProperty), Binding.bindAction(Button::onActionProperty, TodoList::create));

		Element todoVBox = new Element(mainVBox, VBox.class, Binding.forEach(TodoList::getTodos));
		Element todoHBox = new Element(todoVBox, HBox.class);
		Element todoLabel = new Element(todoHBox, Label.class, Binding.bindProperty(Label::textProperty, Todo::getObservable));
		Element todoRemoveButton = new Element(todoHBox, Button.class, Binding.bindAction(Button::onActionProperty, TodoList::remove, Todo.class), Binding.bindProperty(Button::textProperty, Todo::getRemoveButtonTextProperty));

		return mainVBox.apply(this).getNode();
	}
}
