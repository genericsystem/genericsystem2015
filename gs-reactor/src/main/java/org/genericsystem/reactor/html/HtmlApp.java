package org.genericsystem.reactor.html;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ViewContext.RootViewContext;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends ModelContext> extends Tag<M> {

	private final ServerWebSocket webSocket;
	private RootViewContext<M> rootViewContext;

	public HtmlApp(ServerWebSocket webSocket) {
		super(null, "div");
		this.webSocket = webSocket;
	}

	public HtmlApp<M> init(M rootModelContext) {
		rootViewContext = new RootViewContext<M>(rootModelContext, this, new HtmlDomNode(null));
		return this;
	}

	@Override
	public ServerWebSocket getWebSocket() {
		return webSocket;
	}

	public HtmlDomNode getNodeById(String id) {
		return rootViewContext.getNodeById(id);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		throw new UnsupportedOperationException();
	}
}
