package org.genericsystem.distributed.ui;

import io.vertx.core.http.ServerWebSocket;
import java.util.List;
import java.util.function.Function;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public abstract class HtmlElement<COMPONENT extends HtmlElement<COMPONENT, NODE>, NODE extends HtmlDomNode> extends Element<NODE> {

	protected <PARENTNODE extends HtmlDomNode> HtmlElement(Element<PARENTNODE> parent, Class<NODE> nodeClass) {
		super(parent, nodeClass);
	}

	public ServerWebSocket getWebSocket() {
		return ((HtmlElement<?, ?>) getParent()).getWebSocket();
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Function<NODE, List> getGraphicChildren() {
		return NODE::getChildren;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M extends Model, T extends Model> COMPONENT forEach(Function<M, ObservableList<T>> applyOnModel) {
		super.forEach(applyOnModel);
		return (COMPONENT) this;
	}

	@SuppressWarnings({ "unchecked" })
	public <M> COMPONENT setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(HtmlDomNode::getStyleClasses, function);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setStyleClass(String text) {
		addObservableListBoot(HtmlDomNode::getStyleClasses, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <M> COMPONENT bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(HtmlDomNode::getStyleClasses, function, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setText(String text) {
		addBoot(HtmlDomNode::getText, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <M> COMPONENT bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	// @SuppressWarnings("unchecked")
	// public <M> COMPONENT bindReverseText(Function<M, Property<String>> applyOnModel) {
	// addReversedBinding(HtmlDomNode::getText, applyOnModel);
	// return (COMPONENT) this;
	// }

	@SuppressWarnings("unchecked")
	public <M> COMPONENT bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(HtmlDomNode::getText, applyOnModel);
		return (COMPONENT) this;
	}
}
