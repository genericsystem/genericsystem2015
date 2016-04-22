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

/**
 * @author Nicolas Feybesse
 *
 */
public class TodoApp extends HtmlApp<TodoList> {

	public TodoApp(Engine engine, ServerWebSocket webSocket) {
		super(new TodoList(engine), webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv<TodoList> div = new HtmlDiv<>(this);
		{
			HtmlSection<TodoList> todoapp = new HtmlSection<TodoList>(div).addStyleClass("todoapp");
			{
				HtmlHeader<TodoList> header = new HtmlHeader<TodoList>(todoapp).addStyleClass("header");
				{
					new HtmlH1<TodoList>(header).setText("todos");
					new HtmlInputText<TodoList>(header).addStyleClass("new-todo").bindAction(TodoList::create).bindTextBidirectional(TodoList::getName);
				}
				HtmlSection<TodoList> main = new HtmlSection<TodoList>(todoapp).addStyleClass("main");
				{
					HtmlUl<TodoList> todoList = new HtmlUl<TodoList>(main).addStyleClass("todo-list");
					{
						HtmlLi<Todo> li = new HtmlLi<Todo>(todoList).forEach(TodoList::getFiltered).bindOptionalStyleClass(Todo::getCompleted, "completed");
						{
							HtmlDiv<Todo> todoDiv = new HtmlDiv<Todo>(li).addStyleClass("view");
							{
								new HtmlCheckBox<Todo>(todoDiv).addStyleClass("toggle").bindCheckedBidirectional(Todo::getCompleted);
								new HtmlLabel<Todo>(todoDiv).bindText(Todo::getTodoString);
								new HtmlButton<Todo>(todoDiv).addStyleClass("destroy").bindAction(Todo::remove);
							}
						}
					}
				}
				HtmlFooter<TodoList> footer = new HtmlFooter<TodoList>(todoapp).addStyleClass("footer").bindOptionalStyleClass(TodoList::getHasNoTodo, "hide");
				{
					HtmlSpan<TodoList> span = new HtmlSpan<TodoList>(footer).addStyleClass("todo-count");
					{
						new HtmlStrong<TodoList>(span).bindText(TodoList::getActiveCount);
						new HtmlSpan<TodoList>(span).bindText(TodoList::getItems);
					}

					HtmlUl<TodoList> filters = new HtmlUl<TodoList>(footer).addStyleClass("filters");
					{
						new HtmlHyperLink<TodoList>(new HtmlLi<TodoList>(filters), "All", TodoList::showAll).bindOptionalStyleClass(TodoList::getAllMode, "selected");
						new HtmlHyperLink<TodoList>(new HtmlLi<TodoList>(filters), "Actives", TodoList::showActive).bindOptionalStyleClass(TodoList::getActiveMode, "selected");
						new HtmlHyperLink<TodoList>(new HtmlLi<TodoList>(filters), "Completes", TodoList::showCompleted).bindOptionalStyleClass(TodoList::getCompletedMode, "selected");
					}
					new HtmlButton<TodoList>(footer).bindAction(TodoList::removeCompleted).bindText(TodoList::getClearCompleted).addStyleClass("clear-completed").bindOptionalStyleClass(TodoList::getHasNoCompleted, "hide");

				}
			}
		}
	}
}
