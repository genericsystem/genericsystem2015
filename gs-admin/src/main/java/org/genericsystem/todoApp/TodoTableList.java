package org.genericsystem.todoApp;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.util.Callback;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTableItem;
import org.genericsystem.ui.components.GSTableColumn;
import org.genericsystem.ui.components.GSTableView;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class TodoTableList {

	private Property<String> name = new SimpleStringProperty();
	private ObservableList<Todo> todos = FXCollections.observableArrayList();
	// private ObservableList<Column> columns = FXCollections.observableArrayList(new Column(), new DeleteColumn());
	private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Todo");
	private ObservableValue<Number> height = new SimpleDoubleProperty(200);

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

	public void create() {
		Todo todo = new Todo(this);
		todo.stringProperty.setValue(name.getValue());

		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public static class Todo {
		private Property<String> stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);
		private TodoTableList list;

		public Todo(TodoTableList list) {
			this.list = list;
		}

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}

		public void action() {
			System.out.println("ça marche");
		}

		public void remove() {
			list.todos.remove(this);
		}
	}

	public void action() {
		System.out.println("VBox Create");
	}

	public static void init(Element<Group> sceneElt) {
		GSVBox mainVBox = new GSVBox(sceneElt, Group::getChildren).setPrefHeight(600);
		GSHBox todoCreateHBox = new GSHBox(mainVBox);
		GSTextField textField = new GSTextField(todoCreateHBox).bindTextProperty(TodoTableList::getName);
		textField.setPrefWidth(170);

		GSButton todosCreateButton = new GSButton(todoCreateHBox, "Create Todo", TodoTableList::create).setPrefWidth(170);

		GSTableView tableView = new GSTableView(mainVBox);
		GSTableItem<Todo> items = new GSTableItem<>(tableView, Todo.class);
		items.addForEachMetaBinding(TodoTableList::getTodos);

		Callback<CellDataFeatures<Todo, String>, ObservableValue<String>> callback = features -> new SimpleObjectProperty<>(features.getValue().getObservable().getValue());
		GSTableColumn column = new GSTableColumn(tableView).setText("Todo").setWidth(150);
		column.setCellValueFactoryProperty(callback);

		Callback<TableColumn<Todo, String>, TableCell<Todo, String>> callbackDelete = col -> new DeleteButtonCell<>(Todo::remove);
		GSTableColumn columnDelete = new GSTableColumn(tableView).setText("Delete").setWidth(150);
		columnDelete.setCellFactoryProperty(callbackDelete).setCellValueFactoryProperty(callback);
	}
}
