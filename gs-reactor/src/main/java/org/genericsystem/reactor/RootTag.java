package org.genericsystem.reactor;

import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;

import io.vertx.core.http.ServerWebSocket;

public interface RootTag extends Tag {

	default RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}

	@Override
	default RootTag getRootTag() {
		return this;
	}

	AnnotationsManager getAnnotationsManager();

	TagNode buildTagNode(Tag child);
}