package org.genericsystem.distributed.ui.components;

import io.vertx.core.http.ServerWebSocket;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.distributed.ui.ViewContext.RootViewContext;

public class HtmlApp extends HtmlElement<HtmlApp, HtmlNode> {

	private final ServerWebSocket webSocket;
	private final RootViewContext<HtmlNode> rootViewContext;

	public HtmlApp(Model model, ServerWebSocket webSocket) {
		super(HtmlNode.class);
		this.webSocket = webSocket;
		initHtmlChildren();
		rootViewContext = new RootViewContext<>(model, this, new HtmlNode(webSocket, "div"));
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
