package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Tag.RootTag;

import io.vertx.core.http.ServerWebSocket;

public class GSApp extends GSDiv implements RootTag {

	public GSApp() {
		super((Tag) null);
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}

}
