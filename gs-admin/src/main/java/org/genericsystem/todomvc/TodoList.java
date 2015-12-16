package org.genericsystem.todomvc;

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

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSHyperLink;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class TodoList {

	private Property<String> name = new SimpleStringProperty();
	private Property<Predicate<Todo>> mode = new SimpleObjectProperty<>(ALL);
	private ObservableList<Todo> todos = FXCollections.<Todo> observableArrayList(todo -> new Observable[] { todo.getCompleted() });
	private FilteredList<Todo> filtered = new FilteredList<>(todos);
	private ObservableNumberValue completedCount = Bindings.size(todos.filtered(COMPLETE));
	private ObservableValue<String> clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
	private ObservableValue<Boolean> hasCompleted = Bindings.lessThan(0, completedCount);
	private ObservableValue<Boolean> hasTodo = Bindings.lessThan(0, Bindings.size(todos));
	private ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, ALL);
	private ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, ACTIVE);
	private ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, COMPLETE);
	private Property<Todo> selection = new SimpleObjectProperty<>();

	/*********************************************************************************************************************************/

	public static void init(Element<Group> sceneElt) {
		GSVBox mainVBox = new GSVBox(sceneElt, Group::getChildren).setPrefHeight(600);
		{
			GSHBox todosCreation = new GSHBox(mainVBox);
			{
				new GSTextField(todosCreation, TodoList::getName).setPrefWidth(200).bindTextProperty(TodoList::getName);
				new GSButton(todosCreation, "Create Todo", TodoList::create).setPrefWidth(160);
			}

			new GSHBox(mainVBox).forEach(TodoList::getFiltered,/* Todo::getParentProperty, */Todo::init);

			GSHBox todosFiltrage = new GSHBox(mainVBox).setOptionalVisibility(TodoList::getHasTodo);
			{
				new GSHyperLink(todosFiltrage, "All", TodoList::showAll).setOptionalStyleClass(TodoList::getAllMode, "overrun");
				new GSHyperLink(todosFiltrage, "Actives", TodoList::showActive).setOptionalStyleClass(TodoList::getActiveMode, "overrun");
				new GSHyperLink(todosFiltrage, "Completes", TodoList::showCompleted).setOptionalStyleClass(TodoList::getCompletedMode, "overrun");
				new GSButton(todosFiltrage, TodoList::getClearButtonText, TodoList::removeCompleted).setOptionalVisibility(TodoList::getHasCompleted).setPrefWidth(160);
			}

			GSHBox selectionContext = new GSHBox(mainVBox).select(TodoList::getSelection);
			{
				new GSLabel(selectionContext, Todo::getTodoString);
			}
		}
	}

	/*********************************************************************************************************************************/

	public TodoList() {
		filtered.predicateProperty().bind(Bindings.createObjectBinding(() -> mode.getValue(), mode));
	}

	public void create() {
		todos.add(new Todo(getName().getValue()));
	}

	public void showAll() {
		mode.setValue(ALL);
	}

	public void showActive() {
		mode.setValue(ACTIVE);
	}

	public void showCompleted() {
		mode.setValue(COMPLETE);
	}

	public void removeCompleted() {
		for (Todo todo : new ArrayList<>(todos.filtered(COMPLETE)))
			todos.remove(todo);
	}

	static Predicate<Todo> ALL = todo -> true;
	static Predicate<Todo> ACTIVE = todo -> !todo.getCompleted().getValue();
	static Predicate<Todo> COMPLETE = todo -> todo.getCompleted().getValue();

	/*********************************************************************************************************************************/

	public Property<String> getName() {
		return name;
	}

	public Property<Predicate<Todo>> getMode() {
		return mode;
	}

	public ObservableList<Todo> getTodos() {
		return todos;
	}

	public ObservableList<Todo> getFiltered() {
		return filtered;
	}

	public ObservableValue<Number> getCompletedCount() {
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
