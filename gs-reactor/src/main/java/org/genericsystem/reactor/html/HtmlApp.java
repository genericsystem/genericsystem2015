package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ViewContext.RootViewContext;

import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends Tag<M> {

	private final ServerWebSocket webSocket;
	private RootViewContext<M> rootViewContext;
	private M rootModelContext;

	public HtmlApp(ServerWebSocket webSocket) {
		super(null, "div");
		this.webSocket = webSocket;
	}

	public HtmlApp<M> init(M rootModelContext) {
		this.rootModelContext = rootModelContext;
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
