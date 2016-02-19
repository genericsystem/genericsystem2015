package org.genericsystem.distributed.cacheonserver.ui.js.components;

import io.vertx.core.http.ServerWebSocket;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.Model;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;
import org.genericsystem.distributed.cacheonserver.ui.js.ViewContext.RootViewContext;

public class HtmlApplication extends HtmlElement {

	private final ServerWebSocket webSocket;
	private final RootViewContext<HtmlNode> rootViewContext;

	public HtmlApplication(Model model, HtmlNode parentNode, ServerWebSocket webSocket) {
		super(HtmlNode.class);
		this.webSocket = webSocket;
		initHtmlChildren();
		rootViewContext = new RootViewContext<>(model, this, parentNode);
	}

	@Override
	protected void initChildren() {

	}

	protected void initHtmlChildren() {
		super.initChildren();
	}

	@Override
	public ServerWebSocket getWebSocket() {
		return webSocket;
	}

	public RootViewContext<HtmlNode> getRootViewContext() {
		return rootViewContext;
	}
}
