package org.genericsystem.todoApp;

import java.util.Arrays;
import java.util.function.Predicate;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.genericsystem.ui.Binding;
import org.genericsystem.ui.Boot;
import org.genericsystem.ui.Element;

public class TodoList {

	private Property<String> name = new SimpleStringProperty();
	private Property<Mode> mode = new SimpleObjectProperty<>(Mode.ALL);
	private ObservableList<Todo> todos = FXCollections.<Todo> observableArrayList();
	private ObservableList<Todo> filtered = todos.filtered(mode.getValue().predicate());

	private ObservableNumberValue completedCount = Bindings.size(todos.filtered(Mode.COMPLETE.predicate()));
	private ObservableValue<Boolean> hasCompleted = Bindings.lessThan(0, completedCount);
	private ObservableValue<Boolean> hasTodo = Bindings.lessThan(0, Bindings.size(todos));
	private ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue) mode, Mode.ALL);
	private ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue) mode, Mode.ACTIVE);
	private ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue) mode, Mode.COMPLETE);

	public Property<String> getName() {
		return name;
	}

	public ObservableList<Todo> getTodos() {
		return todos;
	}

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public void showAll() {
		mode.setValue(Mode.ALL);
	}

	public void showActive() {
		mode.setValue(Mode.ACTIVE);
	}

	public void showCompleted() {
		mode.setValue(Mode.COMPLETE);
	}

	public static class Todo {
		private Property<String> stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);
		private Property<Boolean> completed = new SimpleBooleanProperty(false);

		public ObservableValue<String> getTodoString() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}

		public Property<Boolean> getCompleted() {
			return completed;
		}
	}

	public Node init() {

		Element<VBox> mainVBox = new Element<>(null, VBox.class);
		mainVBox.addBoots(Boot.setProperty(VBox::prefHeightProperty, 600));

		Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);
		Element<TextField> todoInputText = new Element<>(todoCreateHBox, TextField.class, Binding.bindInputText(TextField::textProperty, TodoList::getName));
		todoInputText.addBoots(Boot.setProperty(TextField::prefWidthProperty, 160));
		Element<Button> todosCreateButton = new Element<>(todoCreateHBox, Button.class, Binding.bindAction(Button::onActionProperty, TodoList::create));
		todosCreateButton.addBoots(Boot.setProperty(Button::textProperty, "Create Todo"), Boot.setProperty(Button::prefWidthProperty, 160));

		Element<HBox> todoHBox = new Element<>(mainVBox, HBox.class, VBox::getChildren, Arrays.asList(Binding.forEach(TodoList::getTodos)));
		Element<CheckBox> todoCheckBox = new Element<>(todoHBox, CheckBox.class, Binding.bindReversedProperty(CheckBox::selectedProperty, Todo::getCompleted));
		Element<Label> todoLabel = new Element<>(todoHBox, Label.class, Binding.bindProperty(Label::textProperty, Todo::getTodoString));
		todoLabel.addBoots(Boot.setProperty(Label::prefWidthProperty, 160));
		Element<Button> todoRemoveButton = new Element<>(todoHBox, Button.class, Binding.bindAction(Button::onActionProperty, TodoList::remove, Todo.class), Binding.bindProperty(Button::textProperty, Todo::getRemoveButtonTextProperty));
		todoRemoveButton.addBoots(Boot.setProperty(Button::prefWidthProperty, 160));
		return mainVBox.apply(this);
	}

	private interface Mode {
		Predicate<Todo> predicate();

		static Mode ALL = new All();
		static Mode ACTIVE = new Active();
		static Mode COMPLETE = new Complete();

		static class All implements Mode {
			@Override
			public Predicate<Todo> predicate() {
				return todo -> true;
			}
		}

		static class Active implements Mode {
			@Override
			public Predicate<Todo> predicate() {
				return todo -> !todo.completed.getValue();
			}
		}

		static class Complete implements Mode {
			@Override
			public Predicate<Todo> predicate() {
				return todo -> todo.completed.getValue();
			}
		}
	};

}
