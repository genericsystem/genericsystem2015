package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ViewContext.RootViewContext;
import org.genericsystem.reactor.appserver.PersistentApplication.App;

import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends HtmlSection<M> implements App<M> {

	private RootViewContext<M> rootViewContext;

	public HtmlApp() {
		super(null);
	}

	@Override
	public HtmlApp<M> init(M rootModelContext, String rootId, ServerWebSocket webSocket) {
		rootViewContext = new RootViewContext<M>(rootModelContext, this, rootId, webSocket);
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
