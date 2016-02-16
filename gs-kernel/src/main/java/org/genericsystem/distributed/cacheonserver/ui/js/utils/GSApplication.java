package org.genericsystem.distributed.cacheonserver.ui.js.utils;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;
import org.genericsystem.distributed.cacheonserver.ui.js.ViewContext.RootViewContext;

public class GSApplication extends HtmlElement {

	private final ServerWebSocket webSocket;
	private final RootViewContext<NodeJs> rootViewContext;

	public GSApplication(Model model, NodeJs parentNode, ServerWebSocket webSocket) {
		super(NodeJs.class);
		this.webSocket = webSocket;
		initChildren();
		rootViewContext = new RootViewContext<>(model, this, parentNode);
	}

	public ServerWebSocket getWebSocket() {
		return webSocket;
	}

	public RootViewContext<NodeJs> getRootViewContext() {
		return rootViewContext;
	}
}
