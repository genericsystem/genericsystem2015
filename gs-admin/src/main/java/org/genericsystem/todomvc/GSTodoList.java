package org.genericsystem.todomvc;

import javafx.scene.Group;

import org.genericsystem.distributed.ui.Model;
import org.genericsystem.ui.components.GSApplication;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSHyperLink;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;

public class GSTodoList extends GSApplication {

	public GSTodoList(Model model, Group parentNode) {
		super(model, parentNode);
	}

	@Override
	protected void initChildren() {
		GSVBox mainVBox = new GSVBox(this, Group::getChildren).setPrefHeight(600);// .setStyleClass("overrun");
		{
			GSHBox todosCreation = new GSHBox(mainVBox);
			{
				new GSTextField(todosCreation, TodoList::getName).setPrefWidth(200).bindTextProperty(TodoList::getName);
				new GSButton(todosCreation, "Create Todo", TodoList::create).setPrefWidth(160);
			}

			new GSTodo(mainVBox).forEach(TodoList::getFiltered);

			GSHBox todosFiltrage = new GSHBox(mainVBox).setOptionalVisibility(TodoList::getHasTodo);
			{
				new GSHyperLink(todosFiltrage, "All", TodoList::showAll).setOptionalStyleClass(TodoList::getAllMode, "overrun");
				new GSHyperLink(todosFiltrage, "Actives", TodoList::showActive).setOptionalStyleClass(TodoList::getActiveMode, "overrun");
				new GSHyperLink(todosFiltrage, "Completes", TodoList::showCompleted).setOptionalStyleClass(TodoList::getCompletedMode, "overrun");
				new GSButton(todosFiltrage, TodoList::getClearButtonText, TodoList::removeCompleted).setOptionalVisibility(TodoList::getHasCompleted).setPrefWidth(160);
			}

			GSHBox selectionContext = new GSHBox(mainVBox).select(TodoList::getSelection);
			{
				new GSLabel(selectionContext, Todo::getTodoString);
			}
		}
	}
}
