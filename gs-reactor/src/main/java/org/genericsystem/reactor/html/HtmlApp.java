package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ViewContext.RootViewContext;

import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends HtmlElement<M, HtmlDomNode> {

	private final ServerWebSocket webSocket;
	private RootViewContext<?, HtmlDomNode> rootViewContext;

	public HtmlApp(ServerWebSocket webSocket) {
		super(null, HtmlDomNode.class);
		this.webSocket = webSocket;
	}

	public HtmlApp<M> init(M model) {
		rootViewContext = new RootViewContext<>(model, (Element) this, new HtmlDomNode("div"));
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
	protected HtmlDomNode createNode(Object parent) {
		throw new UnsupportedOperationException();
	}
}
