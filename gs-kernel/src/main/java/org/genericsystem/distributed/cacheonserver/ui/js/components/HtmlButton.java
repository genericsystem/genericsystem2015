package org.genericsystem.distributed.cacheonserver.ui.js.components;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;

public class HtmlButton extends HtmlElement {

	public HtmlButton(HtmlElement parent, ServerWebSocket webSocket) {
		super(parent, webSocket);
	}

	@Override
	protected NodeJs createNode(Object parent) {
		NodeJs button = new NodeJs('c');
		button.getTag().set("button");
		return button;
	}
}
