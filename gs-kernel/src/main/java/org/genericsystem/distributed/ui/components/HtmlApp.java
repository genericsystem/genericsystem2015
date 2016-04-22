package org.genericsystem.distributed.ui.components;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.ui.Element;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.HtmlDomNode;
import org.genericsystem.distributed.ui.Model;
import org.genericsystem.distributed.ui.ViewContext.RootViewContext;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends HtmlElement<M, HtmlApp<M>, HtmlDomNode> {

	private final ServerWebSocket webSocket;
	private final RootViewContext<?, HtmlDomNode> rootViewContext;

	public HtmlApp(M model, ServerWebSocket webSocket) {
		super(null, HtmlDomNode.class);
		this.webSocket = webSocket;
		initHtmlChildren();
		rootViewContext = new RootViewContext<>(model, (Element) this, new HtmlDomNode("div"));
	}

	@Override
	protected void initChildren() {

	}

	protected void initHtmlChildren() {
		super.initChildren();
	}

	@Override
	public ServerWebSocket getWebSocket() {
		return webSocket;
	}

	public HtmlDomNode getNodeById(String id) {
		return rootViewContext.getNodeById(id);
	}

}
