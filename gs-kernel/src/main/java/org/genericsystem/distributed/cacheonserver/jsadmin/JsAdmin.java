package org.genericsystem.distributed.cacheonserver.jsadmin;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlButton;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlDiv;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlInputText;
import org.genericsystem.distributed.cacheonserver.ui.js.components.HtmlText;
import org.genericsystem.distributed.cacheonserver.ui.js.utils.GSApplication;

public class JsAdmin extends GSApplication {

	public JsAdmin(Model model, NodeJs parentNode, ServerWebSocket webSocket) {
		super(model, parentNode, webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this, getWebSocket());
		{
			HtmlInputText inputText = (HtmlInputText) new HtmlInputText(div, getWebSocket()).addActionBinding(NodeJs::getActionProperty, TodoList::test);

			{
				new HtmlText(inputText, getWebSocket()).addBinding(NodeJs::getTag, TodoList::getName);
			}

			HtmlButton button = (HtmlButton) new HtmlButton(div, getWebSocket()).addActionBinding(NodeJs::getActionProperty, TodoList::test);
			{
				new HtmlText(button, getWebSocket()).addBoot(NodeJs::getTag, "Add");
			}
		}

		HtmlDiv todoList = (HtmlDiv) new HtmlDiv(this, getWebSocket()).forEach(TodoList::getFiltered);
		{
			new HtmlText(todoList, getWebSocket()).addBinding(NodeJs::getTag, Todo::getTodoString);
		}

	}
}
