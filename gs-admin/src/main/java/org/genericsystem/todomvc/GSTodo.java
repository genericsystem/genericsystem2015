package org.genericsystem.todomvc;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSCheckBox;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;

public class GSTodo extends GSHBox {

	public GSTodo(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		new GSCheckBox(this, Todo::getCompleted);
		new GSLabel(this, Todo::getTodoString).setPrefWidth(141).setOptionalStyleClass(Todo::getCompleted, "completed");
		new GSButton(this, "select").setAction(Todo::select).setPrefWidth(90);
		new GSButton(this, "remove").setAction(Todo::remove).setPrefWidth(90);
		// new GSButton(this, "select").setMetaAction((todoList, todo) -> ((TodoList) todoList).getSelection().setValue((Todo) todo)).setPrefWidth(90);
		// new GSButton(this, "remove").setMetaAction((todoList, todo) -> ((TodoList) todoList).getTodos().remove(todo)).setPrefWidth(90);
	}
}
