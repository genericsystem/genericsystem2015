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
import org.genericsystem.ui.components.GSTableButtonColumn;
import org.genericsystem.ui.components.GSTableColumn;
import org.genericsystem.ui.components.GSTableView;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class TodoTableList {

	public static void init(Element<Group> sceneElt) {
		GSVBox mainVBox = new GSVBox(sceneElt, Group::getChildren).setPrefHeight(600);
		{
			GSHBox todoCreateHBox = new GSHBox(mainVBox);
			{
				new GSTextField(todoCreateHBox).bindTextProperty(TodoTableList::getName).setPrefWidth(250);
				new GSButton(todoCreateHBox, "Create Todo", TodoTableList::create).setPrefWidth(250);
				new GSButton(todoCreateHBox, "Create Column", TodoTableList::createColumn).setPrefWidth(250);
			}

			GSTableView tableView = new GSTableView(mainVBox);
			tableView.setObservableList(TableView::itemsProperty, TodoTableList::getTodos);
			{
				Function<Todo, String> converter = todo -> todo.stringProperty.getValue();
				new GSTableColumn<Todo>(tableView, "Todo", converter).setPrefWidth(150);
				new GSTableColumn<>(tableView, AttributeColumn::getTitle, converter).setPrefWidth(150).addForEachMetaBinding(TodoTableList::getColumns);
				new GSTableButtonColumn<>(tableView, "Delete", converter, TodoTableList::remove).setPrefWidth(150);
			}
		}
	}

	private Property<String> name = new SimpleStringProperty();
	private ObservableList<Todo> todos = FXCollections.observableArrayList();
	private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Todo");
	private ObservableValue<Number> height = new SimpleDoubleProperty(200);
	private ObservableList<AttributeColumn> columns = FXCollections.observableArrayList();

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public void createColumn() {
		AttributeColumn ac = new AttributeColumn();
		columns.add(ac);
		ac.title.setValue("Col : " + columns.indexOf(ac));
	}

	public static class Todo {

		private Property<String> stringProperty = new SimpleStringProperty();

		public ObservableValue<String> getStringProperty() {
			return stringProperty;
		}
	}

	/*********************************************************************************************************************************/

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
}
