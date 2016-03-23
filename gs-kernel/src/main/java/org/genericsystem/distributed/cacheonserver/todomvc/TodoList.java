package org.genericsystem.distributed.cacheonserver.todomvc;

import java.util.ArrayList;
import java.util.function.Predicate;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableBooleanValue;
import javafx.beans.value.ObservableNumberValue;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableStringValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.distributed.ui.Transformation2;
import org.genericsystem.kernel.Engine;

@SuppressWarnings("unchecked")
public class TodoList extends Model {

	private final Engine engine;

	private final Property<String> name = new SimpleStringProperty();
	private final Property<Predicate<Todo>> mode = new SimpleObjectProperty<>(ALL);
	private final ObservableList<Todo> todos;// = FXCollections.<Todo> observableArrayList(todo -> new Observable[] { todo.getCompleted() });
	private final FilteredList<Todo> filtered;// = new FilteredList<>(todos);
	private final ObservableNumberValue completedCount;// = Bindings.size(todos.filtered(COMPLETE));
	private final ObservableNumberValue activeCount;// = Bindings.size(todos.filtered(ACTIVE));
	private final ObservableValue<String> clearButtonText;// = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
	private final ObservableValue<Boolean> hasNoCompleted;// = Bindings.equal(0, completedCount);
	private final ObservableBooleanValue hasTodo;// = Bindings.lessThan(0, Bindings.size(todos));
	private final ObservableValue<Boolean> hasNoTodo;// = Bindings.not(hasTodo);
	private final ObservableValue<Boolean> allMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, ALL);
	private final ObservableValue<Boolean> activeMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, ACTIVE);
	private final ObservableValue<Boolean> completedMode = Bindings.equal((ObservableObjectValue<Predicate<Todo>>) mode, COMPLETE);
	private final Property<Todo> selection = new SimpleObjectProperty<>();
	private final ObservableStringValue items;// = Bindings.createStringBinding(() -> " " + (activeCount.getValue().intValue() > 1 ? "items" : "item") + " left", activeCount);
	private final ObservableStringValue clearCompleted;// = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue().intValue() + ")", completedCount);

	public TodoList(Engine engine) {
		this.engine = engine;
		todos = new Transformation2<>(engine.getObservableSubInstances(), g -> {
			Todo todo = new Todo(this, g);
			return todo;
		});
		todos.addListener((ListChangeListener) c -> System.out.println("TRANSFORMATION CHANGE :::" + " " + c));
		filtered = new FilteredList<>(todos);
		filtered.predicateProperty().bind(Bindings.createObjectBinding(() -> mode.getValue(), mode));
		completedCount = Bindings.size(todos.filtered(COMPLETE));
		activeCount = Bindings.size(todos.filtered(ACTIVE));
		clearButtonText = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue() + ")", completedCount);
		hasNoCompleted = Bindings.equal(0, completedCount);
		hasTodo = Bindings.lessThan(0, Bindings.size(todos));
		hasNoTodo = Bindings.not(hasTodo);
		items = Bindings.createStringBinding(() -> " " + (activeCount.getValue().intValue() > 1 ? "items" : "item") + " left", activeCount);
		clearCompleted = Bindings.createStringBinding(() -> "Clear completed (" + completedCount.getValue().intValue() + ")", completedCount);
	}

	public ObservableValue<String> getActiveCount() {
		return Bindings.convert(activeCount);
	}

	public ObservableValue<String> getItems() {
		return items;
	}

	public void create() {
		engine.addInstance(getName().getValue());
		System.out.println("Add instance : " + getName().getValue());
		// todos.add(new Todo(this, getName().getValue()));
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
	public Engine getEngine() {
		return engine;
	}

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
