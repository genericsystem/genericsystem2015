package org.genericsystem.todoCommun;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.todoKernel.Element;
import org.genericsystem.todoList.TodoList;
import org.genericsystem.ui.bindings.OneShotBindings;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSCheckBox;
import org.genericsystem.ui.components.GSLabel;

public class Todo {

	public static void init(Element<HBox> todoHBox) {
		GSCheckBox todoCheckBox = new GSCheckBox(todoHBox, Todo::getCompleted);
		GSLabel todoLabel = new GSLabel(todoHBox, Todo::getTodoString).setPrefWidth(141).setOptionalStyleClass(Todo::getCompleted, "completed");
		GSButton todoSelectButton = new GSButton(todoHBox, "select").setMetaAction(TodoList::select).setPrefWidth(90);
		// GSButton todoRemoveButton = new GSButton(todoHBox, "remove", Todo::remove).setPrefWidth(90);
		// GSButton todoRemoveButton2 = new GSButton(todoHBox, "remove", (MultiConsumer.<TodoList, Todo> create((todolist, todo) -> todolist.todos.remove(todo)))).setPrefWidth(90);
		GSButton todoRemoveButton2 = new GSButton(todoHBox, "remove").setMetaAction(TodoList::remove).setPrefWidth(90);
		todoRemoveButton2.setMetaAction(TodoList::remove);
	}

	Property<TodoList> parentProperty = new SimpleObjectProperty<TodoList>();
	ObservableValue<String> todoString = OneShotBindings.createInitializer(parentProperty, todolist -> todolist.getName().getValue());
	public Property<Boolean> completed = new SimpleBooleanProperty(false);

	/*********************************************************************************************************************************/

	public Property<TodoList> getParentProperty() {
		return parentProperty;
	}

	public ObservableValue<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}

	// public void select() {
	// parentProperty.getValue().selection.setValue(this);
	// }

	// public void remove() {
	// parentProperty.getValue().todos.remove(this);
	// }

	// public static class MultiConsumer<Model, SuperModel> implements Consumer<Object> {
	// private Model model;
	// private BiConsumer<SuperModel, Model> biConsumer;
	//
	// public static <SuperModel, Model> Consumer<Object> create(BiConsumer<SuperModel, Model> biConsumer) {
	// return new MultiConsumer<Model, SuperModel>(biConsumer);
	// }
	//
	// private MultiConsumer(BiConsumer<SuperModel, Model> biConsumer) {
	// this.biConsumer = biConsumer;
	// }
	//
	// @Override
	// public void accept(Object o) {
	// if (model == null) {
	// model = (Model) o;
	// throw new ClassCastException();
	// }
	// biConsumer.accept((SuperModel) o, model);
	// model = null;
	// }
	// }

}
