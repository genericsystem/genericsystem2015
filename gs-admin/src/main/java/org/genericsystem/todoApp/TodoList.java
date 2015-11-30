package org.genericsystem.todoApp;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;

import org.genericsystem.todoApp.binding.Binding;

public class TodoList {

	private StringProperty name = new SimpleStringProperty();
	private ObservableList<Todo> todos = FXCollections.observableArrayList();

	public StringProperty getName() {
		return name;
	}

	public ObservableList<Todo> getTodos() {
		return todos;
	}

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.set(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public static class Todo {
		private StringProperty stringProperty = new SimpleStringProperty();

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}
	}

	public Node init() {

		Element todosVBox = new Element(null, VBox.class, "");

		// Element todoTableView = new Element(todosVBox, TableView.class, "", Binding.forEach(TodoList::getTodos));

		Element todoVox = new Element(todosVBox, VBox.class, "", Binding.forEach(TodoList::getTodos));
		Element todoLabel = new Element(todoVox, Label.class, "", Binding.bindText(Label::textProperty, Todo::getObservable));
		Element todoRemoveButton = new Element(todoVox, Button.class, "remove", Binding.bindAction(Button::onActionProperty, TodoList::remove, Todo.class));

		Element todosCreatLabel = new Element(todosVBox, TextField.class, "", Binding.bindInputText(TextField::textProperty, TodoList::getName));
		Element todosCreateButton = new Element(todosVBox, Button.class, "create", Binding.bindAction(Button::onActionProperty, TodoList::create));

		return todosVBox.apply(this).getNode();
	}
}
