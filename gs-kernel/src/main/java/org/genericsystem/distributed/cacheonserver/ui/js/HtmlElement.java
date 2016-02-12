package org.genericsystem.distributed.cacheonserver.ui.js;

import java.util.function.Function;

import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.cacheonserver.ui.js.utils.Utils;

import io.vertx.core.http.ServerWebSocket;
import javafx.collections.ObservableList;

public class HtmlElement extends Element<NodeJs>{
	private final char type;
	
	
	public HtmlElement(Class<NodeJs> nodeClass) {
		super(nodeClass);
		type='R';
	}

	public HtmlElement(HtmlElement parent, char type, ServerWebSocket webSocket) {
		super(parent, NodeJs.class, Utils.getClassChildren(parent, webSocket));
		this.type=type;
	}

	public char getType() {
		return type;
	}
	
	@Override
	protected NodeJs createNode(Object parent) {
		return new NodeJs(this.type);
	}
}
