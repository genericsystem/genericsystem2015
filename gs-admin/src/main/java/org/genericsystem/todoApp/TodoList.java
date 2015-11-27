package org.genericsystem.todoApp;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import org.genericsystem.todoApp.binding.Binder;
import org.genericsystem.todoApp.binding.Binding;

public class TodoList {

	public StringProperty name = new SimpleStringProperty();
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

		private ObjectProperty<String> stringProperty = new SimpleObjectProperty<>();

		public ObjectProperty<String> getStringProperty() {
			return stringProperty;
		}
	}

	public Node init() {
		Element todosVBox = new Element(null, VBox.class, "");
		Element todoVox = new Element(todosVBox, VBox.class, "", Binding.bindToField(TodoList.class, "todos", Binder.foreach()));
		// Element todoHBox = new Element(todosVBox, VBox.class, "", Binding.bindToMethod(TodoList.class, TodoList::getTodos, ForeachBinder.foreach()));
		Element todoLabel = new Element(todoVox, Label.class, "", Binding.bindToMethod(Todo.class, Todo::getStringProperty, Binder.textBind()));
		Element todoRemoveButton = new Element(todoVox, Button.class, "remove", Binding.bindToMethod(TodoList.class, "remove", Binder.methodBind(), Todo.class));

		Element todosCreatLabel = new Element(todosVBox, TextField.class, "", Binding.bindToField(TodoList.class, "name", Binder.inputTextBind()));
		Element todosCreateButton = new Element(todosVBox, Button.class, "create", Binding.bindToMethod(TodoList.class, "create", Binder.methodBind()));

		return todosVBox.apply(this).getNode();
	}
}
