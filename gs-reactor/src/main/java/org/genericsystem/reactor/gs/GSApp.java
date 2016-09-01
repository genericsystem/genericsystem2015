package org.genericsystem.reactor.gs;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.Tag.RootTag;

public class GSApp extends GSDiv implements RootTag {

	public GSApp() {
		super(null, FlexDirection.COLUMN);
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}

	@Override
	protected HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		throw new UnsupportedOperationException();
	}
}
