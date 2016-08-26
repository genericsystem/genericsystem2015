package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.model.GenericModel;

import io.vertx.core.http.ServerWebSocket;

public class GSApp extends GSSection implements RootTag<GenericModel> {

	public GSApp() {
		super(null, FlexDirection.COLUMN);
	}

	@Override
	public RootHtmlDomNode<GenericModel> init(GenericModel rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode<GenericModel>(rootModelContext, this, rootId, webSocket);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		throw new UnsupportedOperationException();
	}
}
