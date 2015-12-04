package org.genericsystem.todoApp;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.util.Callback;

import org.genericsystem.ui.Binding;
import org.genericsystem.ui.Boot;
import org.genericsystem.ui.Element;

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

	// public ObservableList<Column> getColumns() {
	// return columns;
	// }

	public ObservableValue<String> getCreateButtonTextProperty() {
		return createButtonTextProperty;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	public void create() {
		Todo todo = new Todo();
		todo.stringProperty.setValue(name.getValue());
		todos.add(todo);
	}

	public void remove(Todo todo) {
		this.todos.remove(todo);
	}

	public static class Todo {
		private Property<String> stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}

		public void action() {
			System.out.println("ça marche");
		}
	}

	// public static class Column extends TableColumn<Todo, String> {
	// public Column() {
	// super("Todos");
	// setMinWidth(130);
	// cellValueFactoryProperty();
	// setCellValueFactory(features -> new ReadOnlyObjectWrapper<String>(features.getValue().getObservable().getValue()));
	// }
	//
	// }
	//
	// public static class DeleteColumn extends Column {
	// public DeleteColumn() {
	// setText("Delete");
	// setMinWidth(130);
	// setCellFactory(column -> new DeleteButtonCell<>());
	// }
	// }

	public void action() {
		System.out.println("VBox Create");
	}

	public Node init() {

		Callback<CellDataFeatures<Todo, String>, ObservableValue<String>> callback = features -> new SimpleObjectProperty<>(features.getValue().getObservable().getValue());
		Callback<TableColumn<Todo, String>, TableCell<Todo, String>> callbackDelete = column -> new DeleteButtonCell<>();

		Element<VBox> mainVBox = new Element<>(null, VBox.class);
		mainVBox.addBoots(Boot.setProperty(VBox::prefHeightProperty, 600));
		// mainVBox.addBinding(Binding.setProperty(VBox::prefHeightProperty, 400));

		Element<HBox> todoCreateHBox = new Element<>(mainVBox, HBox.class);
		Element<TextField> todosCreatLabel = new Element<>(todoCreateHBox, TextField.class);
		todosCreatLabel.addBinding(Binding.bindInputText(TextField::textProperty, TodoTableList::getName));

		Element<Button> todosCreateButton = new Element<>(todoCreateHBox, Button.class);
		todosCreateButton.addBinding(Binding.bindProperty(Button::textProperty, TodoTableList::getCreateButtonTextProperty));
		todosCreateButton.addBinding(Binding.bindAction(Button::onActionProperty, TodoTableList::create));

		Element<TableView> todoTableView = new Element<>(mainVBox, TableView.class);

		Function<TableView<Todo>, ObservableList<?>> getItems = TableView::getItems;

		Consumer<Todo> s = Todo::action;

		Element<Todo> todoTableItems = new Element<>(todoTableView, Todo.class, getItems, Arrays.asList(Binding.forEach(TodoTableList::getTodos)));
		todoTableItems.addBoots(Boot.executeMethod(Todo::action));
		Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;

		// Element columnsTableItems = new Element(todoTableView, TableColumn.class, getColumns, Arrays.asList(Binding.forEach(TodoTableList::getColumns)), Binding.setProperty(Column::prefWidthProperty, 100), Binding.setProperty(
		// TableColumn<Todo, String>::cellValueFactoryProperty, callback));

		Element<TableColumn> columnTodo = new Element<>(todoTableView, TableColumn.class, getColumns);
		columnTodo.addBoots(Boot.setProperty(TableColumn<Todo, String>::prefWidthProperty, 100));
		columnTodo.addBoots(Boot.setProperty(TableColumn<Todo, String>::textProperty, "Todo"));
		columnTodo.addBoots(Boot.setProperty(TableColumn<Todo, String>::cellValueFactoryProperty, callback));

		Element<TableColumn> columnDeleteTodo = new Element<>(todoTableView, TableColumn.class, getColumns);

		columnDeleteTodo.addBoots(Boot.setProperty(TableColumn<Todo, String>::prefWidthProperty, 150));
		columnDeleteTodo.addBoots(Boot.setProperty(TableColumn<Todo, String>::textProperty, "Delete todo"));
		columnDeleteTodo.addBoots(Boot.setProperty(TableColumn<Todo, String>::cellValueFactoryProperty, callback), Boot.setProperty(TableColumn<Todo, String>::cellFactoryProperty, callbackDelete));

		return (Node) mainVBox.apply(this).getNode();
	}
}
