package org.genericsystem.distributed.cacheonserver.jsadmin;

import io.vertx.core.http.ServerWebSocket;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlApplication;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlButton;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlDiv;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlInputText;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlText;

public class HtmlAdmin extends HtmlApplication {

	public HtmlAdmin(Model model, HtmlNode parentNode, ServerWebSocket webSocket) {
		super(model, parentNode, webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this);
		{
			HtmlInputText inputText = (HtmlInputText) new HtmlInputText(div).addActionBinding(HtmlNode::getActionProperty, TodoList::test);

			{
				new HtmlText(inputText).addBinding(HtmlNode::getTag, TodoList::getName);
			}

			HtmlButton button = (HtmlButton) new HtmlButton(div).addActionBinding(HtmlNode::getActionProperty, TodoList::test);
			{
				new HtmlText(button).addBoot(HtmlNode::getTag, "Add");
			}
		}

		HtmlDiv todoList = (HtmlDiv) new HtmlDiv(this).forEach(TodoList::getFiltered);
		{
			new HtmlText(todoList).addBinding(HtmlNode::getTag, Todo::getTodoString);
		}

	}
}
