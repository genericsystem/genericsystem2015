package org.genericsystem.todoApp;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import org.genericsystem.todoApp.binding.Binders.ClickBinder;
import org.genericsystem.todoApp.binding.Binders.EnterBinder;
import org.genericsystem.todoApp.binding.Binders.ForeachBinder;
import org.genericsystem.todoApp.binding.Binders.TextBinder;
import org.genericsystem.todoApp.binding.Binding;

public class TodoList {

	public StringProperty name = new SimpleStringProperty();
	public ObservableList<Todo> todos = FXCollections.observableArrayList();

	public void create() {
		Todo todo = new Todo(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public Node init() throws IllegalArgumentException, IllegalAccessException {

		Field attributeTodos = null;
		Field nameAttribute = null;
		Field attributeTodo = null;
		Method methodRemove = null;
		Method methodCreate = null;
		try {
			nameAttribute = TodoList.class.getField("name");
			attributeTodos = TodoList.class.getField("todos");
			attributeTodo = Todo.class.getField("stringProperty");
			methodRemove = TodoList.class.getMethod("remove", Todo.class);
			methodCreate = TodoList.class.getMethod("create");
		} catch (NoSuchFieldException | SecurityException | NoSuchMethodException e) {
			e.printStackTrace();
		}

		Element elmVBoxRoot = new Element(VBox.class, "");
		Element elmVBox = new Element(VBox.class, "", Binding.bindTo(attributeTodos, ForeachBinder.foreach()));
		Element elmLabel = new Element(Label.class, "", Binding.bindTo(attributeTodo, TextBinder.textBind()));
		Element elmButtonRemove = new Element(Button.class, "remove", Binding.bindTo(methodRemove, ClickBinder.methodBind()));
		Element elmButtonCreate = new Element(Button.class, "create", Binding.bindTo(methodCreate, ClickBinder.methodBind()));
		Element elmTextField = new Element(TextField.class, "", Binding.bindTo(nameAttribute, EnterBinder.enterBind()));

		elmVBox.getChildren().add(elmLabel);
		elmVBox.getChildren().add(elmButtonRemove);
		elmVBoxRoot.getChildren().add(elmTextField);
		elmVBoxRoot.getChildren().add(elmButtonCreate);
		elmVBoxRoot.getChildren().add(elmVBox);

		return elmVBoxRoot.apply(this).node;
	}
}
