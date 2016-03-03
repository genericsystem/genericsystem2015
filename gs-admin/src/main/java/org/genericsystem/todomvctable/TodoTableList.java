package org.genericsystem.todomvctable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.ui.Model;

public class TodoTableList extends Model {

	private Property<String> name = new SimpleStringProperty();
	private ObservableList<Todo> todos = FXCollections.observableArrayList();
	private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Todo");
	private ObservableValue<Number> height = new SimpleDoubleProperty(200);
	private ObservableList<TodoColumn> columns = FXCollections.observableArrayList();

	/*********************************************************************************************************************************/

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public void createColumn() {
		TodoColumn ac = new TodoColumn();
		columns.add(ac);
		ac.title.setValue("Col : " + columns.indexOf(ac));
	}

	/*********************************************************************************************************************************/

	public ObservableList<TodoColumn> getColumns() {
		return columns;
	}

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
}
