package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.ViewContext.RootViewContext;
import org.genericsystem.reactor.appserver.PersistentApplication.App;
import org.genericsystem.reactor.model.GenericModel;

import io.vertx.core.http.ServerWebSocket;

public class GSApp extends GSSection implements App<GenericModel> {

	private RootViewContext<GenericModel> rootViewContext;

	public GSApp() {
		super(null, FlexDirection.COLUMN);
	}

	@Override
	public GSApp init(GenericModel rootModelContext, String rootId, ServerWebSocket webSocket) {
		rootViewContext = new RootViewContext<GenericModel>(rootModelContext, this, rootId, webSocket);
		return this;
	}

	@Override
	public HtmlDomNode getNodeById(String id) {
		return rootViewContext.getNodeById(id);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		throw new UnsupportedOperationException();
	}
}
