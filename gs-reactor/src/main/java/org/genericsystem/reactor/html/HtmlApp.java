package org.genericsystem.reactor.html;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ViewContext.RootViewContext;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends HtmlSection<M> {

	private final ServerWebSocket webSocket;
	private RootViewContext<M> rootViewContext;

	public HtmlApp(ServerWebSocket webSocket) {
		super(null);
		this.webSocket = webSocket;
	}

	public HtmlApp<M> init(M rootModelContext, String rootId) {
		HtmlDomNode rootNode = new HtmlDomNode(rootId);
		rootNode.sendAdd(0);
		rootViewContext = new RootViewContext<M>(rootModelContext, this, rootNode);
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
