package org.genericsystem.distributed.cacheonserver.todomvc;

import io.vertx.core.http.ServerWebSocket;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlCheckBox;
import org.genericsystem.distributed.ui.components.HtmlDiv;
import org.genericsystem.distributed.ui.components.HtmlFooter;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlHeader;
import org.genericsystem.distributed.ui.components.HtmlHyperLink;
import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlLi;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.components.HtmlSpan;
import org.genericsystem.distributed.ui.components.HtmlStrong;
import org.genericsystem.distributed.ui.components.HtmlUl;
import org.genericsystem.kernel.Engine;

public class TodoApp extends HtmlApp {

	public TodoApp(Engine engine, ServerWebSocket webSocket) {
		super(new TodoList(engine), webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this);
		{
			HtmlSection todoapp = new HtmlSection(div).setStyleClass("todoapp");
			{
				HtmlHeader header = new HtmlHeader(todoapp).setStyleClass("header");
				{
					new HtmlH1(header).setText("todos");
					new HtmlInputText(header).setStyleClass("new-todo").bindAction(TodoList::create).bindTextBidirectional(TodoList::getName);
				}
				HtmlSection main = new HtmlSection(todoapp).setStyleClass("main");
				{
					HtmlUl todoList = new HtmlUl(main).setStyleClass("todo-list");
					{
						HtmlLi li = new HtmlLi(todoList).forEach(TodoList::getFiltered).bindOptionalStyleClass(Todo::getCompleted, "completed");
						{
							HtmlDiv todoDiv = new HtmlDiv(li).setStyleClass("view");
							{
								new HtmlCheckBox(todoDiv).setStyleClass("toggle").bindCheckedBidirectional(Todo::getCompleted);
								new HtmlLabel(todoDiv).bindText(Todo::getTodoString);
								new HtmlButton(todoDiv).setStyleClass("destroy").bindAction(Todo::remove);
							}
						}
					}
				}
				HtmlFooter footer = new HtmlFooter(todoapp).setStyleClass("footer").bindOptionalStyleClass(TodoList::getHasNoTodo, "hide");
				{
					HtmlSpan span = new HtmlSpan(footer).setStyleClass("todo-count");
					{
						new HtmlStrong(span).bindText(TodoList::getActiveCount);
						new HtmlSpan(span).bindText(TodoList::getItems);
					}

					HtmlUl filters = new HtmlUl(footer).setStyleClass("filters");
					{
						new HtmlHyperLink(new HtmlLi(filters), "All", TodoList::showAll).bindOptionalStyleClass(TodoList::getAllMode, "selected");
						new HtmlHyperLink(new HtmlLi(filters), "Actives", TodoList::showActive).bindOptionalStyleClass(TodoList::getActiveMode, "selected");
						new HtmlHyperLink(new HtmlLi(filters), "Completes", TodoList::showCompleted).bindOptionalStyleClass(TodoList::getCompletedMode, "selected");
					}
					new HtmlButton(footer).bindAction(TodoList::removeCompleted).bindText(TodoList::getClearCompleted).setStyleClass("clear-completed").bindOptionalStyleClass(TodoList::getHasNoCompleted, "hide");

				}
			}
		}
	}
}
