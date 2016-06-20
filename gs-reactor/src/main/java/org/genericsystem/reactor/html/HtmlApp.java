package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ViewContext.RootViewContext;

import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends Element<M> {

	private final ServerWebSocket webSocket;
	private RootViewContext<M> rootViewContext;

	public HtmlApp(ServerWebSocket webSocket) {
		super(null, "div");
		this.webSocket = webSocket;
	}

	public HtmlApp<M> init(M model) {
		rootViewContext = new RootViewContext<M>(model, this, new HtmlDomNode(null));
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
