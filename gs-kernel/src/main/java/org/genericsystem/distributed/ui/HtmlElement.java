package org.genericsystem.distributed.ui;

import io.vertx.core.http.ServerWebSocket;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.ui.utils.Utils;

public abstract class HtmlElement<COMPONENT extends HtmlElement<COMPONENT>> extends Element<COMPONENT, HtmlNode> {

	public HtmlElement(HtmlElement<?> parent) {
		super(parent, HtmlNode.class, Utils.getClassChildren(parent));
	}

	public HtmlElement(Class<HtmlNode> nodeClass) {
		super(nodeClass, HtmlNode::getChildrenNode);
	}

	public ServerWebSocket getWebSocket() {
		return ((HtmlElement<?>) getParent()).getWebSocket();
	}

	@Override
	public <M extends Model, T extends Model> COMPONENT forEach(Function<M, ObservableList<T>> applyOnModel) {
		return super.forEach(applyOnModel);
	}

	public <M> COMPONENT setStyleClass(Function<M, ObservableValue<String>> function) {
		return addObservableListToObservableValueBinding(HtmlNode::getStyleClass, function);
	}

	public COMPONENT setStyleClass(String text) {
		return addObservableListBoot(HtmlNode::getStyleClass, text);
	}

	public <M> COMPONENT setOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		return addObservableListBinding(HtmlNode::getStyleClass, function, text);
	}

}
