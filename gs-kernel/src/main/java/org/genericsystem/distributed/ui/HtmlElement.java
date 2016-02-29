package org.genericsystem.distributed.ui;

import io.vertx.core.http.ServerWebSocket;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.ui.utils.Utils;

public class HtmlElement extends Element<HtmlNode> {

	public HtmlElement(HtmlElement parent) {
		super(parent, HtmlNode.class, Utils.getClassChildren(parent));

	}

	public HtmlElement(Class<HtmlNode> nodeClass) {
		super(nodeClass, HtmlNode::getChildrenNode);
	}

	public ServerWebSocket getWebSocket() {
		return ((HtmlElement) getParent()).getWebSocket();
	}

	@Override
	public <M extends Model, T extends Model> Element<HtmlNode> forEach(Function<M, ObservableList<T>> applyOnModel) {
		return super.forEach(applyOnModel);
	}

	public <M> HtmlElement setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(HtmlNode::getStyleClass, function);
		return this;
	}

	public HtmlElement setStyleClass(String text) {
		addObservableListBoot(HtmlNode::getStyleClass, text);
		return this;
	}
}
