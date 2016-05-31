package org.genericsystem.todomvc;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.ApplicationsDeploymentConfig;
import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlDiv;
import org.genericsystem.reactor.html.HtmlFooter;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlHeader;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.html.HtmlInputText;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlLi;
import org.genericsystem.reactor.html.HtmlSectionTemplate.HtmlSection;
import org.genericsystem.reactor.html.HtmlSpan;
import org.genericsystem.reactor.html.HtmlStrong;
import org.genericsystem.reactor.html.HtmlUl;

/**
 * @author Nicolas Feybesse
 *
 */
public class TodoApp extends HtmlApp<TodoList> {

	public static void main(String[] args) {
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig();
		appsConfig.addApplication("/todomvc", TodoApp.class, TodoList.class, Engine.class, System.getenv("HOME") + "/genericsystem/todo/", Todos.class);
		// appsConfig.addApplication("/second", AppHtml.class, AppModel.class, "/home/middleware/cars/", Car.class,
		// Power.class, Color.class, CarColor.class);
		// apps.addApplication("/todos", TodoApp.class, "/home/middleware/todos/", Todos.class);
		new ApplicationServer(appsConfig).start();
	}

	public TodoApp(AbstractRoot engine, ServerWebSocket webSocket) {
		super(webSocket);
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
						new HtmlHyperLink<TodoList>(new HtmlLi<TodoList>(filters), "All", TodoList::showAll).bindOptionalStyleClass(TodoList::getAllMode,
								"selected");
						new HtmlHyperLink<TodoList>(new HtmlLi<TodoList>(filters), "Actives", TodoList::showActive).bindOptionalStyleClass(
								TodoList::getActiveMode, "selected");
						new HtmlHyperLink<TodoList>(new HtmlLi<TodoList>(filters), "Completes", TodoList::showCompleted).bindOptionalStyleClass(
								TodoList::getCompletedMode, "selected");
					}
					new HtmlButton<TodoList>(footer).bindAction(TodoList::removeCompleted).bindText(TodoList::getClearCompleted)
							.addStyleClass("clear-completed")
							.bindOptionalStyle(TodoList::getCompletedCount, "color", new String[] { "gray", "red", "blue", "black", "green", "yellow" });
					// .bindOptionalStyle(TodoList::getHasNoCompleted, "font", "26px 'Helvetica Neue', Helvetica, Arial, sans-serif");
					// .bindOptionalStyleClass(TodoList::getHasNoCompleted, "hide");

				}
			}
		}
	}
}
