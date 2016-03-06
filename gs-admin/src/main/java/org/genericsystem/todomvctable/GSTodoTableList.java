package org.genericsystem.todomvctable;

import java.util.function.Function;

import javafx.scene.Group;

import org.genericsystem.distributed.ui.Model;
import org.genericsystem.ui.components.GSApplication;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTableButtonColumn;
import org.genericsystem.ui.components.GSTableColumn;
import org.genericsystem.ui.components.GSTableView;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class GSTodoTableList extends GSApplication {

	public GSTodoTableList(Model model, Group parentNode) {
		super(model, parentNode);
	}

	@Override
	protected void initChildren() {
		GSVBox mainVBox = new GSVBox(this).setPrefHeight(600);
		{
			GSHBox todoCreateHBox = new GSHBox(mainVBox);
			{
				new GSTextField(todoCreateHBox).bindTextProperty(TodoTableList::getName).setPrefWidth(250);
				new GSButton(todoCreateHBox, "Create Todo", TodoTableList::create).setPrefWidth(250);
				new GSButton(todoCreateHBox, "Create Column", TodoTableList::createColumn).setPrefWidth(250);
			}

			GSTableView tableView = new GSTableView(mainVBox);
			tableView.setObservableListItems(TodoTableList::getTodos);
			{
				Function<Todo, String> converter = todo -> todo.stringProperty.getValue();
				new GSTableColumn<Todo>(tableView, "Todo", converter).setPrefWidth(150);
				new GSTableColumn<>(tableView, TodoColumn::getTitle, converter).setPrefWidth(150).forEach(TodoTableList::getColumns);
				new GSTableButtonColumn<>(tableView, "Delete", converter, TodoTableList::remove).setPrefWidth(150);
			}
		}
	}

}
