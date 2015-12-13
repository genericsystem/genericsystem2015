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
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHyperLink;

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
		Element<VBox> mainVBox = new Element<>(sceneElt, VBox.class, Group::getChildren);
		mainVBox.addBoot(VBox::prefHeightProperty, 600);

		Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);
		Element<TextField> todoInputText = new Element<>(todoCreateHBox, TextField.class);
		todoInputText.addBidirectionalBinding(TextField::textProperty, TodoList::getName);
		todoInputText.addBoot(TextField::prefWidthProperty, 166);
		GSButton todosCreateButton = new GSButton(todoCreateHBox, "Create Todo", TodoList::create).setPrefWidth(160);

		Element<HBox> todoHBox = new Element<>(mainVBox, HBox.class);
		todoHBox.addForEachMetaBinding(TodoList::getFiltered, Todo::getParentProperty);
		Todo.init(todoHBox);

		Element<HBox> footer = new Element<>(mainVBox, HBox.class);
		footer.addBinding(HBox::visibleProperty, TodoList::getHasTodo);

		GSHyperLink allLink = new GSHyperLink(footer, "All", TodoList::showAll).setOptionalStyleClass(TodoList::getAllMode, "overrun");
		GSHyperLink activeLink = new GSHyperLink(footer, "Actives", TodoList::showActive).setOptionalStyleClass(TodoList::getActiveMode, "overrun");
		GSHyperLink completeLink = new GSHyperLink(footer, "Completes", TodoList::showCompleted).setOptionalStyleClass(TodoList::getCompletedMode, "overrun");

		GSButton clearButton = new GSButton(footer, TodoList::getClearButtonText, TodoList::removeCompleted);
		clearButton.setOptionalVisibility(TodoList::getHasCompleted);
		clearButton.setPrefWidth(160);

		Element<HBox> selectionHBox = new Element<>(mainVBox, HBox.class);
		selectionHBox.addSelectorMetaBinding(TodoList::getSelection);
		Element<Label> selectedTodoInputText = new Element<>(selectionHBox, Label.class);
		selectedTodoInputText.addBinding(Label::textProperty, Todo::getTodoString);
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
