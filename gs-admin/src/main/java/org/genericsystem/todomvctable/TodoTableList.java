package org.genericsystem.todomvctable;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.control.TableView;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTableColumn;
import org.genericsystem.ui.components.GSTableColumn.GSTableColumnAction;
import org.genericsystem.ui.components.GSTableView;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class TodoTableList {

	private Property<String> name = new SimpleStringProperty();
	private ObservableList<Todo> todos = FXCollections.observableArrayList();
	// private ObservableList<Column> columns = FXCollections.observableArrayList(new Column(), new DeleteColumn());
	private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Todo");
	private ObservableValue<Number> height = new SimpleDoubleProperty(200);

	private ObservableList<AttributeColumn> columns = FXCollections.observableArrayList();

	public ObservableList<AttributeColumn> getColumns() {
		return columns;
	}

	public static class AttributeColumn {
		Property<String> title = new SimpleObjectProperty<String>();

		public ObservableValue<String> getTitle() {
			return title;
		}
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

	public void create() {
		Todo todo = new Todo(this);
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
		AttributeColumn ac = new AttributeColumn();
		columns.add(ac);
		ac.title.setValue("Col : " + columns.indexOf(ac));
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public static class Todo {

		private Property<String> stringProperty = new SimpleStringProperty();
		private TodoTableList list;

		public Todo(TodoTableList list) {
			this.list = list;
		}

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public void action() {
			System.out.println("ça marche");
		}

		public void remove() {
			list.todos.remove(this);
		}

	}

	public static void init(Element<Group> sceneElt) {
		GSVBox mainVBox = new GSVBox(sceneElt, Group::getChildren).setPrefHeight(600);
		GSHBox todoCreateHBox = new GSHBox(mainVBox);
		GSTextField textField = new GSTextField(todoCreateHBox).bindTextProperty(TodoTableList::getName);
		textField.setPrefWidth(170);

		GSButton todosCreateButton = new GSButton(todoCreateHBox, "Create Todo", TodoTableList::create).setPrefWidth(170);

		GSTableView tableView = new GSTableView(mainVBox);
		tableView.setObservableList(TableView::itemsProperty, TodoTableList::getTodos);
		Function<Todo, String> converter = todo -> todo.stringProperty.getValue();
		GSTableColumn<Todo> column = new GSTableColumn<Todo>(tableView, "Todo", converter).setPrefWidth(150);
		// Function<AttributeColumn, String> converter2 = attributeColumn -> attributeColumn.title.getValue();
		GSTableColumn<AttributeColumn> columns = new GSTableColumn<>(tableView, AttributeColumn::getTitle, converter).setPrefWidth(150);
		columns.addForEachMetaBinding(TodoTableList::getColumns);
		GSTableColumnAction<TodoTableList, Todo> columnDelete = new GSTableColumnAction<>(tableView, "Delete", converter, TodoTableList::remove).setPrefWidth(150);
	}
}
