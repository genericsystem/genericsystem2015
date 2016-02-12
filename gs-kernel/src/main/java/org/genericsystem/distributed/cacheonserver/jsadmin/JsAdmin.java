package org.genericsystem.distributed.cacheonserver.jsadmin;

import io.vertx.core.http.ServerWebSocket;


import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;
import org.genericsystem.distributed.cacheonserver.ui.js.utils.GSApplication;


public class JsAdmin extends GSApplication {

	public JsAdmin(Model model, NodeJs parentNode, ServerWebSocket webSocket) {
		super(model, parentNode, webSocket);
	}

	@Override
	protected void initChildren() {
		HtmlElement div =  (HtmlElement) new HtmlElement(this, 'd', getWebSocket()).addBoot(NodeJs::getTag,"div");
		{
			HtmlElement inputText =  (HtmlElement) new HtmlElement(div, 'd', getWebSocket()).addBoot(NodeJs::getTag,"input");
			{
				HtmlElement text =  (HtmlElement) new HtmlElement(inputText, 't', getWebSocket()).addBinding(NodeJs::getTag, TodoList::getName);
			}
			
			HtmlElement button =  (HtmlElement) new HtmlElement(div, 'd', getWebSocket()).addBoot(NodeJs::getTag,"button");
			{
				HtmlElement textButton =  (HtmlElement) new HtmlElement(button, 't', getWebSocket()).addBoot(NodeJs::getTag,"Add");
			}
		}
		HtmlElement todoList = (HtmlElement) new HtmlElement(this,'d',getWebSocket()).forEach(TodoList::getFiltered).addBoot(NodeJs::getTag,"div");
		{
			HtmlElement label =(HtmlElement) new HtmlElement(todoList,'t',getWebSocket()).addBinding(NodeJs::getTag,Todo::getTodoString);
		}
		
	}

}
