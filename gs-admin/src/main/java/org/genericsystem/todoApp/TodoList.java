package org.genericsystem.todoApp;

import java.util.ArrayList;

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

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSHyperLink;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class TodoList {

	private Property<String> name = new SimpleStringProperty();
	private Property<Mode> mode = new SimpleObjectProperty<>(Mode.ALL);
	ObservableList<Todo> todos = FXCollections.<Todo> observableArrayList(todo -> new Observable[] { todo.getCompleted() });
	private FilteredList<Todo> filtered = new FilteredList<>(todos);
	private ObservableNumberValue completedCount = Bindings.size(todos.filtered(Mode.COMPLETE.predicate()));
	private ObservableValue<String> clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
	private ObservableValue<Boolean> hasCompleted = Bindings.lessThan(0, completedCount);
	private ObservableValue<Boolean> hasTodo = Bindings.lessThan(0, Bindings.size(todos));
	private ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue) mode, Mode.ALL);
	private ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue) mode, Mode.ACTIVE);
	private ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue) mode, Mode.COMPLETE);
	Property<Todo> selection = new SimpleObjectProperty<>();

	public TodoList() {
		filtered.predicateProperty().bind(Bindings.createObjectBinding(() -> mode.getValue().predicate(), mode));
	}

	public void create() {
		todos.add(new Todo());
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

	public static void init(Element<Group> sceneElt) {
		GSVBox mainVBox = new GSVBox(sceneElt, Group::getChildren).setPrefHeight(600);

		GSHBox todoCreateHBox = new GSHBox(mainVBox);
		GSTextField textField = new GSTextField(todoCreateHBox);
		textField.bindTextProperty(TodoList::getName);
		textField.setPrefWidth(200);

		GSButton todosCreateButton = new GSButton(todoCreateHBox, "Create Todo", TodoList::create).setPrefWidth(160);

		GSHBox todoHBox = new GSHBox(mainVBox).addForEachMetaBinding(TodoList::getFiltered, Todo::getParentProperty, Todo::init);

		GSHBox footer = new GSHBox(mainVBox).setOptionalVisibility(TodoList::getHasTodo);
		GSHyperLink allLink = new GSHyperLink(footer, "All", TodoList::showAll).setOptionalStyleClass(TodoList::getAllMode, "overrun");
		GSHyperLink activeLink = new GSHyperLink(footer, "Actives", TodoList::showActive).setOptionalStyleClass(TodoList::getActiveMode, "overrun");
		GSHyperLink completeLink = new GSHyperLink(footer, "Completes", TodoList::showCompleted).setOptionalStyleClass(TodoList::getCompletedMode, "overrun");

		GSButton clearButton = new GSButton(footer, TodoList::getClearButtonText, TodoList::removeCompleted);
		clearButton.setOptionalVisibility(TodoList::getHasCompleted);
		clearButton.setPrefWidth(160);

		GSHBox selectionHBox = new GSHBox(mainVBox);
		selectionHBox.addSelectorMetaBinding(TodoList::getSelection);
		GSLabel selectedTodoInputText = new GSLabel(selectionHBox, Todo::getTodoString);
	}

	public Property<String> getName() {
		return name;
	}

	public Property<Mode> getMode() {
		return mode;
	}

	public ObservableList<Todo> getTodos() {
		return todos;
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
	};

}
