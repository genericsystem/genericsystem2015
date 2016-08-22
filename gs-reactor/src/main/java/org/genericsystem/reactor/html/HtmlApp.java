package org.genericsystem.reactor.html;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.ViewContext.RootViewContext;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlApp<M extends Model> extends HtmlSection<M> implements RootTag<M> {

	public HtmlApp() {
		super(null);
	}

	@Override
	public RootViewContext<M> init(M rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootViewContext<M>(rootModelContext, this, rootId, webSocket);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		throw new UnsupportedOperationException();
	}
}
