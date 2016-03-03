package org.genericsystem.distributed.ui;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.ui.utils.Utils;

public abstract class HtmlElement<COMPONENT extends HtmlElement<COMPONENT>> extends Element<HtmlNode> {

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
		super.forEach(applyOnModel);
		return (COMPONENT) this;
	}

	public <M> COMPONENT setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(HtmlNode::getStyleClass, function);
		return (COMPONENT) this;
	}

	public COMPONENT setStyleClass(String text) {
		addObservableListBoot(HtmlNode::getStyleClass, text);
		return (COMPONENT) this;
	}

	public <M> COMPONENT setOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(HtmlNode::getStyleClass, function, text);
		return (COMPONENT) this;
	}

	public COMPONENT setText(String text) {
		addBoot(HtmlNode::getText, text);
		return (COMPONENT) this;
	}

	public <M> COMPONENT setRWText(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	public <M> COMPONENT setWText(Function<M, Property<String>> applyOnModel) {
		addReversedBinding(HtmlNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	public <M> COMPONENT setText(Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(HtmlNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	// public <M> COMPONENT setText(Function<M, ObservableValue<String>> applyOnModel) {
	// addBidirectionalBinding(HtmlNode::getText, applyOnModel);
	// return (COMPONENT) this;
	// }

	@Override
	public void sendMessage(JsonObject jsonObj) {
		getWebSocket().write(new GSBuffer().appendString(jsonObj.encode()));
	}

}
