package org.genericsystem.todomvc;

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
import org.genericsystem.reactor.html.HtmlSection;
import org.genericsystem.reactor.html.HtmlSpan;
import org.genericsystem.reactor.html.HtmlStrong;
import org.genericsystem.reactor.html.HtmlUl;

import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
public class TodoApp extends HtmlApp<TodoList> {

	public static void main(String[] args) {
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig();
		appsConfig.addApplication("/todomvc", TodoApp.class, TodoList.class, Engine.class, System.getenv("HOME") + "/genericsystem/todo/", Todos.class);
		new ApplicationServer(appsConfig).start();
	}

	public TodoApp(AbstractRoot engine, ServerWebSocket webSocket) {
		super(webSocket);
		new HtmlDiv<TodoList>(this) {
			{
				new HtmlSection<TodoList>(this) {
					{
						addStyleClass("todoapp");
						new HtmlHeader<TodoList>(this) {
							{
								addStyleClass("header");
								new HtmlH1<TodoList>(this).setText("todos");
								new HtmlInputText<TodoList>(this) {
									{
										addStyleClass("new-todo");
										bindAction(TodoList::create);
										bindTextBidirectional(TodoList::getName);
									}
								};
							}
						};
						new HtmlSection<TodoList>(this) {
							{
								addStyleClass("main");
								new HtmlUl<TodoList>(this) {
									{
										addStyleClass("todo-list");
										new HtmlLi<Todo>(this) {
											{

												forEach(TodoList::getFiltered);
												bindOptionalStyleClass(Todo::getCompleted, "completed");
												new HtmlDiv<Todo>(this) {
													{
														addStyleClass("view");
														new HtmlCheckBox<Todo>(this) {
															{
																addStyleClass("toggle");
																bindCheckedBidirectional(Todo::getCompleted);
															}
														};
														new HtmlLabel<Todo>(this) {
															{
																bindText(Todo::getTodoString);
															}
														};
														new HtmlButton<Todo>(this) {
															{
																addStyleClass("destroy");
																bindAction(Todo::remove);
															}
														};
													}
												};

											}
										};
									}
								};
							}
						};

						new HtmlFooter<TodoList>(this) {
							{
								addStyleClass("footer");
								bindOptionalStyleClass(TodoList::getHasNoTodo, "hide");
								new HtmlSpan<TodoList>(this) {
									{
										addStyleClass("todo-count");
										new HtmlStrong<TodoList>(this).bindText(TodoList::getActiveCount);
										new HtmlSpan<TodoList>(this).bindText(TodoList::getItems);
									}
								};
								new HtmlUl<TodoList>(this) {
									{
										addStyleClass("filters");
										new HtmlLi<TodoList>(this) {
											{
												new HtmlHyperLink<TodoList>(this, "All", TodoList::showAll).bindOptionalStyleClass(TodoList::getAllMode,
														"selected");
												;
											}
										};
										new HtmlLi<TodoList>(this) {
											{

												new HtmlHyperLink<TodoList>(this, "Actives", TodoList::showActive)
														.bindOptionalStyleClass(TodoList::getActiveMode, "selected");
												;
											}
										};
										new HtmlLi<TodoList>(this) {
											{
												new HtmlHyperLink<TodoList>(this, "Completes", TodoList::showCompleted)
														.bindOptionalStyleClass(TodoList::getCompletedMode, "selected");
												;
											}
										};
									}
								};
								new HtmlButton<TodoList>(this) {
									{
										addStyleClass("clear-completed");
										bindAction(TodoList::removeCompleted);
										bindText(TodoList::getClearCompleted);
										bindOptionalStyleClass(TodoList::getHasNoCompleted, "hide");
									}
								};
							}
						};
					}
				};
			}
		};
	}
}
