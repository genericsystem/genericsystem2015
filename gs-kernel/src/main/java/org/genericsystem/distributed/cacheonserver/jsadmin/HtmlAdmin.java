package org.genericsystem.distributed.cacheonserver.jsadmin;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlApplication;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlButton;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlDiv;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlInputText;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlLabel;

public class HtmlAdmin extends HtmlApplication {

	public HtmlAdmin(Model model, HtmlNode parentNode, ServerWebSocket webSocket) {
		super(model, parentNode, webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this);
		{
			new HtmlInputText(div).addBidirectionalBinding(HtmlNode::getText, TodoList::getName);

			new HtmlButton(div).addActionBinding(HtmlNode::getActionProperty, TodoList::create).addBoot(HtmlNode::getText, "Add");
		}

		HtmlDiv todoList = (HtmlDiv) new HtmlDiv(this).forEach(TodoList::getFiltered);
		{
			new HtmlLabel(todoList).addBinding(HtmlNode::getText, Todo::getTodoString);
		}

	}
}
