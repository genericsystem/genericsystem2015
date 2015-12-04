package org.genericsystem.todoApp;

import java.util.Arrays;
import java.util.function.Consumer;

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

import org.genericsystem.ui.Binding;
import org.genericsystem.ui.Boot;
import org.genericsystem.ui.Element;

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

		public ObservableValue<String> getTodoString() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}
	}

	public Node init() {

		Consumer<VBox> c = VBox::autosize;

		Element<VBox> mainVBox = new Element<>(null, VBox.class);// , Binding.bindProperty(VBox::prefHeightProperty, TodoList::getHeight));
		mainVBox.addBoots(Boot.setProperty(VBox::prefHeightProperty, 600));
		Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);
		Element<TextField> todosCreatLabel = new Element<>(todoCreateHBox, TextField.class, Binding.bindInputText(TextField::textProperty, TodoList::getName));
		Element<Button> todosCreateButton = new Element<>(todoCreateHBox, Button.class, Binding.bindAction(Button::onActionProperty, TodoList::create));
		todosCreateButton.addBoots(Boot.setProperty(Button::textProperty, "Create Todo"));

		Element<HBox> todoHBox = new Element<HBox>(mainVBox, HBox.class, VBox::getChildren, Arrays.asList(Binding.forEach(TodoList::getTodos)));
		Element<Label> todoLabel = new Element<>(todoHBox, Label.class, Binding.bindProperty(Label::textProperty, Todo::getTodoString));
		Element<Button> todoRemoveButton = new Element<>(todoHBox, Button.class, Binding.bindAction(Button::onActionProperty, TodoList::remove, Todo.class), Binding.bindProperty(Button::textProperty, Todo::getRemoveButtonTextProperty));

		return mainVBox.apply(this).getNode();

		// Element mainVBox = new Element(null, VBox.class, Binding.bindProperty(VBox::prefHeightProperty, TodoList::getHeight));
		// Element todoCreateHBox = new Element(mainVBox, HBox.class);
		// Element todosCreatLabel = new Element(todoCreateHBox, TextField.class, Binding.bindInputText(TextField::textProperty, TodoList::getName));
		// Element todosCreateButton = new Element(todoCreateHBox, Button.class, Binding.bindProperty(Button::textProperty, TodoList::getCreateButtonTextProperty), Binding.bindAction(Button::onActionProperty, TodoList::create));
		//
		// Element todoHBox = new Element(mainVBox, HBox.class, VBox::getChildren, Arrays.asList(Binding.forEach(TodoList::getTodos)));
		//
		// Element todoLabel = new Element(todoHBox, Label.class, Binding.bindProperty(Label::textProperty, Todo::getTodoString));
		// Element todoRemoveButton = new Element(todoHBox, Button.class, Binding.bindAction(Button::onActionProperty, TodoList::remove, Todo.class), Binding.bindProperty(Button::textProperty, Todo::getRemoveButtonTextProperty));
		//
		// return (Node) mainVBox.apply(this).getNode();
	}
}
