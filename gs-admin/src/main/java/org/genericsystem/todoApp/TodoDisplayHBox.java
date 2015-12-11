package org.genericsystem.todoApp;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

import org.genericsystem.todoApp.TodoListElement.IModel;
import org.genericsystem.todoApp.TodoListElement.TodoList;
import org.genericsystem.ui.Element;

public class TodoDisplayHBox extends Element<HBox> {

	public TodoDisplayHBox(Element parent) {

		super(parent, HBox.class);

		Element<CheckBox> todoCheckBox = new Element<>(this, CheckBox.class);

		todoCheckBox.addBidirectionalBinding(CheckBox::selectedProperty, Todo::getCompleted);

		Element<Label> todoLabel = new Element<>(this, Label.class);
		todoLabel.addBinding(Label::textProperty, Todo::getTodoString);
		todoLabel.addBoot(Label::prefWidthProperty, 141);
		todoLabel.addObservableListBinding(Label::getStyleClass, Todo::getCompleted, "completed");

		Element<Button> todoSelectButton = new Element<>(this, Button.class);

		todoSelectButton.addActionBinding(Button::onActionProperty, Todo::select);
		todoSelectButton.addBoot(Button::textProperty, "select");
		todoSelectButton.addBoot(Button::prefWidthProperty, 90);

		Element<Button> todoRemoveButton = new Element<>(this, Button.class);
		todoRemoveButton.addActionBinding(Button::onActionProperty, Todo::remove);
		todoRemoveButton.addBoot(Button::textProperty, "remove");
		todoRemoveButton.addBoot(Button::prefWidthProperty, 90);
	}

	public static class Todo implements IModel<TodoList> {

		TodoList parent;

		public Todo(TodoList list) {
			this.parent = list;
		}

		Property<String> stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);
		Property<Boolean> completed = new SimpleBooleanProperty(false);

		public ObservableValue<String> getTodoString() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}

		public Property<Boolean> getCompleted() {
			return completed;
		}

		public void select() {
			getParent().selection.setValue(this);
		}

		public void remove() {
			System.out.println("zzzz" + parent);
			// TodoList todoList = (getParent());
			parent.remove(this);
		}

		// public Function<TodoList, TodoList> getParent2() {
		// return TodoList::getThis;
		// }
	}

}
