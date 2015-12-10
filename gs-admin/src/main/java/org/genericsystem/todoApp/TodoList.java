package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.function.Predicate;
import javafx.beans.Observable;
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
import javafx.collections.transformation.FilteredList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.genericsystem.ui.Element;

public class TodoList {

	private Property<String> name = new SimpleStringProperty();
	private Property<Mode> mode = new SimpleObjectProperty<>(Mode.ALL);
	private ObservableList<Todo> todos = FXCollections.<Todo> observableArrayList(todo -> new Observable[] { todo.getCompleted() });
	private ObservableValue<Predicate<Todo>> observablePredicate = Bindings.createObjectBinding(() -> mode.getValue().predicate(), mode);
	private FilteredList<Todo> filtered = new FilteredList<>(todos);

	private ObservableNumberValue completedCount = Bindings.size(todos.filtered(Mode.COMPLETE.predicate()));
	private ObservableValue<String> clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
	private ObservableValue<Boolean> hasCompleted = Bindings.lessThan(0, completedCount);
	private ObservableValue<Boolean> hasTodo = Bindings.lessThan(0, Bindings.size(todos));
	private ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue) mode, Mode.ALL);
	private ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue) mode, Mode.ACTIVE);
	private ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue) mode, Mode.COMPLETE);
	private Property<Todo> selection = new SimpleObjectProperty<>();

	public TodoList() {
		filtered.predicateProperty().bind(observablePredicate);
	}

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
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

	public class Todo {
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

		public void select() {
			selection.setValue(this);
		}

		public void remove() {
			todos.remove(this);
		}

	}

	public Node init() {

		Element<VBox> mainVBox = new Element<>(null, VBox.class);
		mainVBox.addBoot(VBox::prefHeightProperty, 600);

		Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);
		Element<TextField> todoInputText = new Element<>(todoCreateHBox, TextField.class);
		todoInputText.addBidirectionalBinding(TextField::textProperty, m -> name);
		todoInputText.addBoot(TextField::prefWidthProperty, 166);
		Element<Button> todosCreateButton = new Element<>(todoCreateHBox, Button.class);
		todosCreateButton.addActionBinding(Button::onActionProperty, TodoList::create);
		todosCreateButton.addBoot(Button::textProperty, "Create Todo");
		todosCreateButton.addBoot(Button::prefWidthProperty, 160);

		Element<HBox> todoHBox = new Element<>(mainVBox, HBox.class);
		// Element<HBox> todoHBox = Elt.createHbox(mainVBox);new Element<HBox>(mainVBox, HBox.class);
		todoHBox.addForEachMetaBinding(m -> filtered);
		Element<CheckBox> todoCheckBox = new Element<>(todoHBox, CheckBox.class);
		todoCheckBox.addBidirectionalBinding(CheckBox::selectedProperty, Todo::getCompleted);
		Element<Label> todoLabel = new Element<>(todoHBox, Label.class);
		todoLabel.addBinding(Label::textProperty, Todo::getTodoString);
		todoLabel.addBoot(Label::prefWidthProperty, 141);
		todoLabel.addObservableListBinding(Label::getStyleClass, Todo::getCompleted, "completed");
		Element<Button> todoSelectButton = new Element<>(todoHBox, Button.class);
		todoSelectButton.addActionBinding(Button::onActionProperty, Todo::select);
		todoSelectButton.addBoot(Button::textProperty, "select");
		todoSelectButton.addBoot(Button::prefWidthProperty, 90);
		Element<Button> todoRemoveButton = new Element<>(todoHBox, Button.class);
		todoRemoveButton.addActionBinding(Button::onActionProperty, Todo::remove);
		todoRemoveButton.addBoot(Button::textProperty, "remove");
		todoRemoveButton.addBoot(Button::prefWidthProperty, 90);

		Element<HBox> footer = new Element<>(mainVBox, HBox.class);
		footer.addBinding(HBox::visibleProperty, (m) -> hasTodo);
		Element<Hyperlink> allLink = new Element<>(footer, Hyperlink.class);
		allLink.addActionBinding(Hyperlink::onActionProperty, TodoList::showAll);
		allLink.addBoot(Hyperlink::textProperty, "All");
		allLink.addObservableListBinding(Hyperlink::getStyleClass, (m) -> allMode, "overrun");
		Element<Hyperlink> activeLink = new Element<>(footer, Hyperlink.class);
		activeLink.addActionBinding(Hyperlink::onActionProperty, TodoList::showActive);
		activeLink.addBoot(Hyperlink::textProperty, "Actives");
		activeLink.addObservableListBinding(Hyperlink::getStyleClass, (m) -> activeMode, "overrun");
		Element<Hyperlink> completeLink = new Element<>(footer, Hyperlink.class);
		completeLink.addActionBinding(Hyperlink::onActionProperty, TodoList::showCompleted);
		completeLink.addBoot(Hyperlink::textProperty, "Completes");
		completeLink.addObservableListBinding(Hyperlink::getStyleClass, (m) -> completedMode, "overrun");
		Element<Button> clearButton = new Element<>(footer, Button.class);
		clearButton.addBinding(Button::visibleProperty, (m) -> hasCompleted);
		clearButton.addActionBinding(Button::onActionProperty, TodoList::removeCompleted);
		clearButton.addBinding(Button::textProperty, m -> clearButtonText);
		clearButton.addBoot(Button::prefWidthProperty, 160);

		Element<HBox> selectionHBox = new Element<>(mainVBox, HBox.class);
		selectionHBox.addSelectorMetaBinding((m) -> selection);
		Element<Label> selectedTodoInputText = new Element<>(selectionHBox, Label.class);
		selectedTodoInputText.addBinding(Label::textProperty, Todo::getTodoString);

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
