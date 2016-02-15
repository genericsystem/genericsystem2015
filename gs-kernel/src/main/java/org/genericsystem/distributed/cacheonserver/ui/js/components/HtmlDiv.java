package org.genericsystem.distributed.cacheonserver.ui.js.components;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;

public class HtmlDiv extends HtmlElement {
	public HtmlDiv(HtmlElement parent, ServerWebSocket webSocket) {
		super(parent, webSocket);
	}

	@Override
	protected NodeJs createNode(Object parent) {
		NodeJs div = new NodeJs('c');
		div.getTag().set("div");
		return div;
	}
}
