package org.genericsystem.distributed.cacheonserver.ui.js;

import io.vertx.core.http.ServerWebSocket;

import java.util.function.Function;

import javafx.collections.ObservableList;

import org.genericsystem.distributed.cacheonserver.ui.js.utils.Utils;

public class HtmlElement extends Element<NodeJs> {
	public HtmlElement(HtmlElement parent, ServerWebSocket webSocket) {
		super(parent, NodeJs.class, Utils.getClassChildren(parent, webSocket));

	}

	public HtmlElement(Class<NodeJs> class1) {
		super(class1);
	}

	@Override
	public <M extends Model, T extends Model> Element<NodeJs> forEach(Function<M, ObservableList<T>> applyOnModel) {
		return super.forEach(applyOnModel);
	}
}
