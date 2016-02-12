package org.genericsystem.distributed.cacheonserver.jsadmin;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.Element;
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
		HtmlElement rootJs2 = new HtmlElement(this, 'd', getWebSocket());
		{
			HtmlElement rootJs4 = new HtmlElement(rootJs2, 'd', getWebSocket());
			HtmlElement rootJs3 = new HtmlElement(rootJs2, 'D', getWebSocket());
		}
		
		HtmlElement rootJs6 = new HtmlElement(this, 'd', getWebSocket());
		{
			HtmlElement rootJs4 = new HtmlElement(rootJs6, 'd', getWebSocket());
			HtmlElement rootJs3 = new HtmlElement(rootJs6, 'D', getWebSocket());//.addBinding(NodeJs::getData, MyModel::getString);
		}
	}

}
