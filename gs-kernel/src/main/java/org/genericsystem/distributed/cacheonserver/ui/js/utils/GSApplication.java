package org.genericsystem.distributed.cacheonserver.ui.js.utils;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.Element;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;
import org.genericsystem.distributed.cacheonserver.ui.js.ViewContext.RootViewContext;

public class GSApplication extends Element<NodeJs> {

	private final ServerWebSocket webSocket;

	public GSApplication(Model model, NodeJs parentNode, ServerWebSocket webSocket) {
		super(NodeJs.class);
		this.webSocket = webSocket;
		initChildren();
		new RootViewContext(model, this, parentNode);
	}

	public ServerWebSocket getWebSocket() {
		return webSocket;
	}
}
