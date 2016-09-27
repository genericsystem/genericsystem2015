package org.genericsystem.reactor.az;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.az3.GSCompositeDiv;

public class GSApp extends GSCompositeDiv implements RootTag {

	public GSApp() {
		super(null);
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}
}
