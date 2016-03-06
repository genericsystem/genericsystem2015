package org.genericsystem.distributed.ui;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.GSBuffer;

public abstract class HtmlElement<COMPONENT extends HtmlElement<COMPONENT, NODE>, NODE extends HtmlNode> extends Element<NODE> {

	protected <PARENTNODE extends HtmlNode> HtmlElement(Element<PARENTNODE> parent, Class<NODE> nodeClass) {
		super(parent, nodeClass);
	}

	public ServerWebSocket getWebSocket() {
		return ((HtmlElement<?, ?>) getParent()).getWebSocket();
	}

	@Override
	protected <PARENTNODE> Function<PARENTNODE, List<NODE>> getGraphicChildren() {
		Function<PARENTNODE, List<NODE>> nodeJsChildren = parentNodeJs -> new AbstractList<NODE>() {
			private List<NODE> childrenNode = new ArrayList<>();

			@Override
			public NODE get(int index) {
				return childrenNode.get(index);
			}

			@Override
			public int size() {
				return childrenNode.size();
			}

			@Override
			public void add(int index, NODE htmlNode) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "A");
				htmlNode.fillJsonAdd((HtmlNode) parentNodeJs, jsonObj);
				getParent().sendMessage(jsonObj);
				childrenNode.add(htmlNode);
			}

			@Override
			public NODE set(int index, NODE element) {
				return childrenNode.set(index, (element));
			}

			@Override
			public NODE remove(int index) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "R");
				childrenNode.get(index).fillJsonRemove(jsonObj);
				getParent().sendMessage(jsonObj);
				return childrenNode.remove(index);
			}
		};

		return nodeJsChildren;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M extends Model, T extends Model> COMPONENT forEach(Function<M, ObservableList<T>> applyOnModel) {
		super.forEach(applyOnModel);
		return (COMPONENT) this;
	}

	@SuppressWarnings({ "unchecked" })
	public <M> COMPONENT setStyleClass(Function<M, ObservableValue<String>> function) {
		addObservableListToObservableValueBinding(HtmlNode::getStyleClasses, function);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setStyleClass(String text) {
		addObservableListBoot(HtmlNode::getStyleClasses, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <M> COMPONENT setOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(HtmlNode::getStyleClasses, function, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setText(String text) {
		addBoot(HtmlNode::getText, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <M> COMPONENT setRWText(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <M> COMPONENT setWText(Function<M, Property<String>> applyOnModel) {
		addReversedBinding(HtmlNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <M> COMPONENT setText(Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(HtmlNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	@Override
	public void sendMessage(JsonObject jsonObj) {
		getWebSocket().write(new GSBuffer().appendString(jsonObj.encode()));
	}

}
