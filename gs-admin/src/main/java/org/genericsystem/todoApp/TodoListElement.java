package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.function.Predicate;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.scene.Group;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.genericsystem.todoApp.TodoDisplayHBox.Todo;
import org.genericsystem.ui.Element;

public class TodoListElement extends Element<Group> {

	public TodoListElement(Group scene) {
		super(Group.class);

		Element<VBox> mainVBox = new Element<>(this, VBox.class, Group::getChildren);
		mainVBox.addBoot(VBox::prefHeightProperty, 600);

		Element<HBox> hboxCreate = new Element<HBox>(mainVBox, HBox.class);
		Element<TextField> todoInputText = new Element<>(hboxCreate, TextField.class);
		todoInputText.addBidirectionalBinding(TextField::textProperty, TodoList::getName);
		todoInputText.addBoot(TextField::prefWidthProperty, 166);
		Element<Button> todosCreateButton = new Element<>(hboxCreate, Button.class);
		todosCreateButton.addActionBinding(Button::onActionProperty, TodoList::create);
		todosCreateButton.addBoot(Button::textProperty, "Create Todo");
		todosCreateButton.addBoot(Button::prefWidthProperty, 160);

		Element<HBox> footer = new Element<>(mainVBox, HBox.class);
		footer.addBinding(HBox::visibleProperty, TodoList::getHasTodo);
		Element<Hyperlink> allLink = new Element<>(footer, Hyperlink.class);
		allLink.addActionBinding(Hyperlink::onActionProperty, TodoList::showAll);
		allLink.addBoot(Hyperlink::textProperty, "All");
		allLink.addObservableListBinding(Hyperlink::getStyleClass, TodoList::getAllMode, "overrun");
		Element<Hyperlink> activeLink = new Element<>(footer, Hyperlink.class);
		activeLink.addActionBinding(Hyperlink::onActionProperty, TodoList::showActive);
		activeLink.addBoot(Hyperlink::textProperty, "Actives");
		activeLink.addObservableListBinding(Hyperlink::getStyleClass, TodoList::getActiveMode, "overrun");
		Element<Hyperlink> completeLink = new Element<>(footer, Hyperlink.class);
		completeLink.addActionBinding(Hyperlink::onActionProperty, TodoList::showCompleted);
		completeLink.addBoot(Hyperlink::textProperty, "Completes");
		completeLink.addObservableListBinding(Hyperlink::getStyleClass, TodoList::getCompletedMode, "overrun");
		Element<Button> clearButton = new Element<>(footer, Button.class);
		clearButton.addBinding(Button::visibleProperty, TodoList::getHasCompleted);
		clearButton.addActionBinding(Button::onActionProperty, TodoList::removeCompleted);
		clearButton.addBinding(Button::textProperty, TodoList::getClearButtonText);
		clearButton.addBoot(Button::prefWidthProperty, 160);

		Element<HBox> selectionHBox = new Element<>(mainVBox, HBox.class);
		selectionHBox.addSelectorMetaBinding(TodoList::getSelection);
		Element<Label> selectedTodoInputText = new Element<>(selectionHBox, Label.class);
		selectedTodoInputText.addBinding(Label::textProperty, Todo::getTodoString);

		Element<HBox> todoHBox = new TodoDisplayHBox(mainVBox);
		todoHBox.addForEachMetaBinding(TodoList::getFiltered);
		this.apply(new TodoList(), scene);
	}

	public static interface IModel<T> {
		default T getParent() {
			System.out.println("eeeeeeee");
			return (T) this;
		}
	}

	public static class TodoList {
		private Property<String> name = new SimpleStringProperty();
		private Property<Mode> mode = new SimpleObjectProperty<>(Mode.ALL);
		public ObservableList<Todo> todos = FXCollections.<Todo> observableArrayList(todo -> new Observable[] { todo.getCompleted() });
		private ObservableValue<Predicate<Todo>> observablePredicate = Bindings.createObjectBinding(() -> mode.getValue().predicate(), mode);
		private FilteredList<Todo> filtered = new FilteredList<>(todos);

		private ObservableNumberValue completedCount = Bindings.size(todos.filtered(Mode.COMPLETE.predicate()));
		private ObservableValue<String> clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
		private ObservableValue<Boolean> hasCompleted = Bindings.lessThan(0, completedCount);
		private ObservableValue<Boolean> hasTodo = Bindings.lessThan(0, Bindings.size(todos));
		private ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue) mode, Mode.ALL);
		private ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue) mode, Mode.ACTIVE);
		private ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue) mode, Mode.COMPLETE);
		public Property<Todo> selection = new SimpleObjectProperty<>();

		public void create() {
			Todo todo = new Todo(this);
			todo.stringProperty.setValue(name.getValue());
			todos.add(todo);
		}

		public ObservableValue<TodoList> getThis() {
			return (ObservableValue<TodoList>) this;
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

		public void removeCompleted() {

			for (Todo todo : new ArrayList<>(todos.filtered(Mode.COMPLETE.predicate())))
				todos.remove(todo);
		}

		public Property<String> getName() {
			return name;
		}

		public Property<Mode> getMode() {
			return mode;
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
		}

		public ObservableList<Todo> getTodos() {
			return todos;
		}

		public ObservableValue<Predicate<Todo>> getObservablePredicate() {
			return observablePredicate;
		}

		public FilteredList<Todo> getFiltered() {
			return filtered;
		}

		public ObservableNumberValue getCompletedCount() {
			return completedCount;
		}

		public ObservableValue<String> getClearButtonText() {
			return clearButtonText;
		}

		public ObservableValue<Boolean> getHasCompleted() {
			return hasCompleted;
		}

		public ObservableValue<Boolean> getHasTodo() {
			return hasTodo;
		}

		public ObservableValue<Boolean> getAllMode() {
			return allMode;
		}

		public ObservableValue<Boolean> getActiveMode() {
			return activeMode;
		}

		public ObservableValue<Boolean> getCompletedMode() {
			return completedMode;
		}

		public Property<Todo> getSelection() {
			return selection;
		}

		public void remove(Todo todo) {
			todos.remove(todo);
		}
	}
}
