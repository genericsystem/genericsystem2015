package org.genericsystem.distributed.cacheonserver.ui.js.components;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;

public class HtmlText extends HtmlElement {

	public HtmlText(HtmlElement parent, ServerWebSocket webSocket) {
		super(parent, webSocket);
	}

	@Override
	protected NodeJs createNode(Object parent) {
		return new NodeJs('t');
	}

}
