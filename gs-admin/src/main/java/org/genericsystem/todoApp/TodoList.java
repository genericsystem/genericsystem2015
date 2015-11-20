package org.genericsystem.todoApp;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.genericsystem.todoApp.IElement.Element;
import org.genericsystem.todoApp.binding.Binders;
import org.genericsystem.todoApp.binding.Binding.BindingImpl;

public class TodoList {

	public ObjectProperty<String> name = new SimpleObjectProperty<String>();
	public ObservableValue<List<Todo>> todos = new SimpleObjectProperty<List<Todo>>(new ArrayList<>());

	public void createTodo(String instance) {
		name.set(instance);
		Todo todo = new Todo(name);
		todos.getValue().add(todo);
	}

	public Node init() throws IllegalArgumentException, IllegalAccessException {

		Field attribute = null;
		try {
			attribute = TodoList.class.getField("todos");
		} catch (NoSuchFieldException | SecurityException e) {
			e.printStackTrace();
		}

		BindingImpl binding = new BindingImpl();
		BindingImpl bindingForeach = binding.bindTo(attribute, Binders.foreachBinder.foreach());

		List<IElement> content = new ArrayList<IElement>();
		IElement elmVBox = new Element(VBox.class, "", bindingForeach, content);
		IElement elmButton = new Element(Button.class, "button1", bindingForeach, null);
		IElement elmButton2 = new Element(Button.class, "button2", bindingForeach, null);
		content.add(elmButton);
		content.add(elmButton2);

		List<IElement> content2 = new ArrayList<IElement>();
		IElement elmVBox2 = new Element(HBox.class, "", bindingForeach, content2);
		IElement elmButton3 = new Element(Button.class, "button3", bindingForeach, null);
		IElement elmButton4 = new Element(Button.class, "button4", bindingForeach, null);
		content2.add(elmButton3);
		content2.add(elmButton4);

		content.add(elmVBox2);
		return elmVBox.apply(this).node;
	}
}
