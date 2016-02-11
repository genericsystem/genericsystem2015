package org.genericsystem.distributed.cacheonserver.jsadmin;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.Element;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;
import org.genericsystem.distributed.cacheonserver.ui.js.utils.GSApplication;

public class JsAdmin extends GSApplication {

	public JsAdmin(Model model, NodeJs parentNode, ServerWebSocket webSocket) {
		super(model, parentNode, webSocket);
	}

	@Override
	protected void initChildren() {
		Element<NodeJs> rootJs = new Element<>(this, NodeJs.class, getWebSocket());
		Element<NodeJs> rootJs2 = new Element<>(this, NodeJs.class, getWebSocket());
	}

}
