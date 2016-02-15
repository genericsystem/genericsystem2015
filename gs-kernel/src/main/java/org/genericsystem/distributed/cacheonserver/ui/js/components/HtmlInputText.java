package org.genericsystem.distributed.cacheonserver.ui.js.components;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;

public class HtmlInputText extends HtmlElement {

	public HtmlInputText(HtmlElement parent, ServerWebSocket webSocket) {
		super(parent, webSocket);
	}

	@Override
	protected NodeJs createNode(Object parent) {
		NodeJs input = new NodeJs('c');
		input.getTag().set("input");
		return input;
	}

}
