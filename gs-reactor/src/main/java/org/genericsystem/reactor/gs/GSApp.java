package org.genericsystem.reactor.gs;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.ViewContext.RootViewContext;
import org.genericsystem.reactor.model.GenericModel;

public class GSApp extends GSSection implements RootTag<GenericModel> {

	public GSApp() {
		super(null, FlexDirection.COLUMN);
	}

	@Override
	public RootViewContext<GenericModel> init(GenericModel rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootViewContext<GenericModel>(rootModelContext, this, rootId, webSocket);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		throw new UnsupportedOperationException();
	}
}
