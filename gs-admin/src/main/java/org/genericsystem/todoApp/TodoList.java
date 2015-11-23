package org.genericsystem.todoApp;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;

import org.genericsystem.todoApp.IElement.Element;
import org.genericsystem.todoApp.binding.Binders;
import org.genericsystem.todoApp.binding.Binding.BindingImpl;

public class TodoList {

	public ObjectProperty<String> name = new SimpleObjectProperty<String>();

	public ObservableValue<List<Todo>> todos = new SimpleObjectProperty<List<Todo>>(new ArrayList<>());

	public void createTodo(String instance) {
		Todo todo = new Todo(instance);
		todos.getValue().add(todo);
	}

	public void removeTodo(Todo todo) {
		todos.getValue().remove(todo);
	}

	public Node init() throws IllegalArgumentException, IllegalAccessException {

		// todos.getValue().forEach(t -> System.out.println(t.stringProperty.get()));

		Field attributeTodos = null;
		Field attributeTodo = null;
		try {
			attributeTodos = TodoList.class.getField("todos");
			attributeTodo = Todo.class.getField("stringProperty");
		} catch (NoSuchFieldException | SecurityException e) {
			e.printStackTrace();
		}

		BindingImpl bindingForeach = new BindingImpl();
		BindingImpl bindingTodo = new BindingImpl();
		BindingImpl bindingRemove = new BindingImpl();

		List<IElement> content = new ArrayList<IElement>();
		IElement elmVBox = new Element(VBox.class, "", bindingForeach.bindTo(attributeTodos, Binders.foreachBinder.foreach()), content);
		IElement elmLabel = new Element(Label.class, "", bindingTodo.bindTo(attributeTodo, Binders.TodoBinder.todoBind()), null);

		// IElement elmButton = new Element(Button.class, "remove", bindingRemove.bindTo(attribute, binder), null);
		content.add(elmLabel);

		return elmVBox.apply(this).node;
	}
}
