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
		
		HtmlElement root = (HtmlElement) new HtmlElement(this,'d',getWebSocket()).forEach(TodoList::getFiltered).addBoot(NodeJs::getTag,"div");
//		root;
		{
			HtmlElement label =(HtmlElement) new HtmlElement(root,'t',getWebSocket()).addBinding(NodeJs::getTag,Todo::getTodoString);
		}
		
//		HtmlElement rootJs2 = (HtmlElement) new HtmlElement(this, 'd', getWebSocket()).addBoot(NodeJs::getTag,"div");
//		{
//			HtmlElement rootJs4 =  (HtmlElement) new HtmlElement(rootJs2, 'd', getWebSocket()).addBoot(NodeJs::getTag,"button");
//			HtmlElement rootJs42 =  (HtmlElement) new HtmlElement(rootJs4, 't', getWebSocket()).addBoot(NodeJs::getTag,"button Test");
//		}
//		
//		HtmlElement rootJs6 =  (HtmlElement) new HtmlElement(this, 'd', getWebSocket()).addBoot(NodeJs::getTag,"div");
//		{
//			HtmlElement rootJs4 = (HtmlElement) new HtmlElement(rootJs6, 'd', getWebSocket()).addBoot(NodeJs::getTag,"div");
//			HtmlElement rootJs3 = (HtmlElement) new HtmlElement(rootJs6, 't', getWebSocket()).addBoot(NodeJs::getTag,"text");
//		}
	}

}
