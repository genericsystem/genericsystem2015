package org.genericsystem.distributed.cacheonserver.todomvc;

import java.util.ArrayList;
import java.util.function.Predicate;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableBooleanValue;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableStringValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

import org.genericsystem.distributed.ui.Model;

@SuppressWarnings("unchecked")
public class TodoList extends Model {

	private Property<String> name = new SimpleStringProperty();
	private Property<Predicate<Todo>> mode = new SimpleObjectProperty<>(ALL);
	private ObservableList<Todo> todos = FXCollections.<Todo> observableArrayList(todo -> new Observable[] { todo.getCompleted() });
	private FilteredList<Todo> filtered = new FilteredList<>(todos);
	private ObservableNumberValue completedCount = Bindings.size(todos.filtered(COMPLETE));
	private ObservableNumberValue activeCount = Bindings.size(todos.filtered(ACTIVE));
	private ObservableValue<String> clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
	private ObservableValue<Boolean> hasCompleted = Bindings.lessThan(0, completedCount);
	private final ObservableValue<Boolean> hasNoCompleted = Bindings.equal(0, completedCount);
	private ObservableBooleanValue hasTodo = Bindings.lessThan(0, Bindings.size(todos));
	private ObservableValue<Boolean> hasNoTodo = Bindings.not(hasTodo);
	private ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, ALL);
	private ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, ACTIVE);
	private ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, COMPLETE);
	private Property<Todo> selection = new SimpleObjectProperty<>();
	ObservableStringValue items = Bindings.createStringBinding(() -> " " + (activeCount.getValue().intValue() > 1 ? "items" : "item") + " left", activeCount);
	private final ObservableStringValue clearCompleted = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue().intValue() + ")", completedCount);

	public TodoList() {
		filtered.predicateProperty().bind(Bindings.createObjectBinding(() -> mode.getValue(), mode));
		// ObservableValue<String> items = Bindings.createStringBinding(()->completedCount.getValue().intValue()>1?"items":"item", completedCount);
		//
		// completedCount.addListener(new ChangeListener<Number>() {
		//
		// @Override
		// public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
		// // TODO Auto-generated method stub
		// System.out.println("ddfdfdffdf");
		// }
		// });
		// items.addListener(new ChangeListener<String>() {
		//
		// @Override
		// public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
		// // TODO Auto-generated method stub
		// System.out.println(newValue);
		// }
		// });
	}

	public ObservableValue<String> getActiveCount() {
		return Bindings.convert(activeCount);
	}

	public ObservableValue<String> getItems() {
		return items;
	}

	public void create() {
		todos.add(new Todo(this, getName().getValue()));
		name.setValue(null);
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

	public ObservableValue<Boolean> getHasNoCompleted() {
		return hasNoCompleted;
	}

	public ObservableValue<Boolean> getHasTodo() {
		return hasTodo;
	}

	public ObservableValue<Boolean> getHasNoTodo() {
		return hasNoTodo;
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

	public ObservableValue<String> getClearCompleted() {
		return clearCompleted;
	};

}
