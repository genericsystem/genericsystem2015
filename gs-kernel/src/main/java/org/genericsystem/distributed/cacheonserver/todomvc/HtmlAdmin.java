package org.genericsystem.distributed.cacheonserver.todomvc;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.ui.HtmlNode;
import org.genericsystem.distributed.ui.Model;
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

public class HtmlAdmin extends HtmlApp {

	public HtmlAdmin(Model model, HtmlNode parentNode, ServerWebSocket webSocket) {
		super(model, parentNode, webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this);
		{
			HtmlSection todoapp = new HtmlSection(div).setStyleClass("todoapp");
			{
				HtmlHeader header = new HtmlHeader(todoapp).setStyleClass("header");
				{
					new HtmlH1(header).addBoot(HtmlNode::getText, "todos");
					new HtmlInputText(header).setStyleClass("new-todo").addActionBinding(HtmlNode::getActionProperty, TodoList::create).addBidirectionalBinding(HtmlNode::getText, TodoList::getName);
				}
				HtmlSection main = new HtmlSection(todoapp).setStyleClass("main");
				{
					HtmlUl todoList = new HtmlUl(main).setStyleClass("todo-list");
					{
						HtmlLi li = new HtmlLi(todoList).forEach(TodoList::getFiltered).setStyleClass("toto").setOptionalStyleClass(Todo::getCompleted, "completed");
						{
							HtmlDiv todoDiv = new HtmlDiv(li).setStyleClass("view");
							{
								new HtmlCheckBox(todoDiv).setStyleClass("toggle").addBidirectionalBinding(HtmlNode::getChecked, Todo::getCompleted);
								new HtmlLabel(todoDiv).addBinding(HtmlNode::getText, Todo::getTodoString);
								new HtmlButton(todoDiv).setStyleClass("destroy").addActionBinding(HtmlNode::getActionProperty, Todo::remove);
							}
						}
					}
				}
				HtmlFooter footer = new HtmlFooter(todoapp).setStyleClass("footer").setOptionalStyleClass(TodoList::getHasNoTodo, "hide");
				{
					HtmlSpan span = new HtmlSpan(footer).setStyleClass("todo-count");
					{
						new HtmlStrong(span).addBinding(HtmlNode::getText, TodoList::getActiveCount);
						new HtmlSpan(span).addBinding(HtmlNode::getText, TodoList::getItems);
					}

					HtmlUl filters = new HtmlUl(footer).setStyleClass("filters");
					{
						new HtmlHyperLink(new HtmlLi(filters), "All", TodoList::showAll).setOptionalStyleClass(TodoList::getAllMode, "selected");
						new HtmlHyperLink(new HtmlLi(filters), "Actives", TodoList::showActive).setOptionalStyleClass(TodoList::getActiveMode, "selected");
						new HtmlHyperLink(new HtmlLi(filters), "Completes", TodoList::showCompleted).setOptionalStyleClass(TodoList::getCompletedMode, "selected");
					}
					new HtmlButton(footer).addActionBinding(HtmlNode::getActionProperty, TodoList::removeCompleted).addBinding(HtmlNode::getText, TodoList::getClearCompleted).setStyleClass("clear-completed")
							.setOptionalStyleClass(TodoList::getHasNoCompleted, "hide");

				}
			}
		}
	}
}
