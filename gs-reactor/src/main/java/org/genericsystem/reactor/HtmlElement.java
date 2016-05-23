package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

import org.genericsystem.common.GSBuffer;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;

/**
 * @author Nicolas Feybesse
 *
 * @param <COMPONENT>
 * @param <NODE>
 */

public abstract class HtmlElement<M extends Model, COMPONENT extends HtmlElement<M, COMPONENT, NODE>, NODE extends HtmlDomNode>
		extends Element<M, NODE> {
	private static final String MSG_TYPE = "msgType";
	private static final String ADD = "A";
	private static final String UPDATE = "U";
	private static final String REMOVE = "R";

	private static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	private static final String NEXT_ID = "nextId";
	private static final String STYLECLASS = "styleClass";
	private static final String TEXT_CONTENT = "textContent";
	private static final String TAG_HTML = "tagHtml";
	private static final String ELT_TYPE = "eltType";

	protected <PARENTMODEL extends Model, PARENTNODE extends HtmlDomNode> HtmlElement(
			Element<PARENTMODEL, PARENTNODE> parent, Class<NODE> nodeClass) {
		super(parent, nodeClass);
	}

	public ServerWebSocket getWebSocket() {
		return ((HtmlElement<?, ?, ?>) getParent()).getWebSocket();
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Function<NODE, List> getGraphicChildren() {
		return HtmlDomNode::getChildren;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends Model> COMPONENT forEach(Function<T, ObservableList<M>> applyOnModel) {
		super.forEach(applyOnModel);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public COMPONENT forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor,
			ModelConstructor<CompositeModel> constructor) {
		super.forEach(stringExtractor, observableListExtractor, constructor);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends Model> COMPONENT select(Function<T, ObservableValue<M>> function) {
		super.select(function);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends CompositeModel> COMPONENT select(Function<T, Property<CompositeModel>> function,
			StringExtractor stringExtractor, Supplier<Generic> generic, ModelConstructor<CompositeModel> constructor) {
		super.select(function, stringExtractor, generic, constructor);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public <T extends CompositeModel> COMPONENT select(Function<T, Property<CompositeModel>> function,
			StringExtractor stringExtractor, Supplier<Generic> generic) {
		select(function, stringExtractor, generic, CompositeModel::new);
		return (COMPONENT) this;
	}

	// @SuppressWarnings({ "unchecked" })
	// public <M> COMPONENT bindStyleClass(Function<M, ObservableValue<String>> function) {
	// addObservableListToObservableValueBinding(HtmlDomNode::getStyleClasses, function);
	// return (COMPONENT) this;
	// }

	@SuppressWarnings("unchecked")
	public COMPONENT addStyleClass(String text) {
		addObservableListBoot(HtmlDomNode::getStyleClasses, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addObservableListBinding(HtmlDomNode::getStyleClasses, function, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setText(String text) {
		addBoot(HtmlDomNode::getText, text);
		return (COMPONENT) this;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	// @SuppressWarnings("unchecked")
	// public <M> COMPONENT bindReverseText(Function<M, Property<String>> applyOnModel) {
	// addReversedBinding(HtmlDomNode::getText, applyOnModel);
	// return (COMPONENT) this;
	// }

	@SuppressWarnings("unchecked")
	public COMPONENT bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(HtmlDomNode::getText, applyOnModel);
		return (COMPONENT) this;
	}

	public class HtmlDomNode {

		private final String id;
		private final String tag;
		private final StringProperty text = new SimpleStringProperty();
		private final ObservableList<String> styleClasses = FXCollections.observableArrayList();

		public HtmlDomNode(String tag) {
			this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
			this.tag = tag;
		}

		public void initListener() {
			this.text.addListener((o, oldValue, newValue) -> {
				sendMessage(new JsonObject().put(MSG_TYPE, UPDATE).put(ID, id).put(TEXT_CONTENT, newValue));
				System.out.println("newValue : " + newValue);
			});

			this.styleClasses.addListener((ListChangeListener<String>) change -> {
				JsonArray arrayJS = new JsonArray();
				styleClasses.forEach(clazz -> arrayJS.add(clazz));
				sendMessage(new JsonObject().put(MSG_TYPE, UPDATE).put(ID, id).put(STYLECLASS, arrayJS));
			});
		}

		List<HtmlDomNode> getChildren() {
			return children;
		}

		private final List<HtmlDomNode> children = new AbstractList<HtmlDomNode>() {
			private final List<HtmlDomNode> internal = new ArrayList<>();

			@Override
			public HtmlDomNode get(int index) {
				return internal.get(index);
			}

			@Override
			public int size() {
				return internal.size();
			}

			@Override
			public void add(int index, HtmlDomNode childNode) {
				JsonObject jsonObj = new JsonObject().put(MSG_TYPE, ADD);
				childNode.fillJson(HtmlDomNode.this, jsonObj);
				jsonObj.put(NEXT_ID, index < size() ? get(index).id : null);
				sendMessage(jsonObj);
				internal.add(index, childNode);
			}

			@Override
			public HtmlDomNode remove(int index) {
				JsonObject jsonObj = new JsonObject().put(MSG_TYPE, REMOVE);
				jsonObj.put(ID, internal.get(index).id);
				sendMessage(jsonObj);
				return internal.remove(index);
			}
		};

		public void sendMessage(JsonObject jsonObj) {
			getWebSocket().write(new GSBuffer().appendString(jsonObj.encode()));
		}

		void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			jsonObj.put(PARENT_ID, parentNodeJs.getId());
			jsonObj.put(ID, id);
			jsonObj.put(TAG_HTML, tag);
			jsonObj.put(TEXT_CONTENT, text.getValue());
			System.out.print("--text value : " + text.getValue());
			JsonArray arrayJS = new JsonArray();
			styleClasses.forEach(arrayJS::add);
			jsonObj.put(STYLECLASS, arrayJS);
		}

		public ObservableList<String> getStyleClasses() {
			return styleClasses;
		}

		public StringProperty getText() {
			return text;
		}

		public String getId() {
			return id;
		}

		public String getTag() {
			return tag;
		}

		public void handleMessage(JsonObject json) {

		}

	}

	public class ActionHtmlNode extends HtmlDomNode {
		private final ObjectProperty<EventHandler<ActionEvent>> actionProperty = new SimpleObjectProperty<>();

		public ActionHtmlNode(String tag) {
			super(tag);
		}

		public ObjectProperty<EventHandler<ActionEvent>> getActionProperty() {
			return actionProperty;
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getActionProperty().get().handle(new ActionEvent());
			super.handleMessage(json);
		}

	}

	public class InputTextHtmlDomNode extends HtmlDomNode {
		private final ObjectProperty<EventHandler<ActionEvent>> enterProperty = new SimpleObjectProperty<>();

		public InputTextHtmlDomNode() {
			super("input");
		}

		@Override
		public void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			super.fillJson(parentNodeJs, jsonObj);
			jsonObj.put("type", "text");
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getEnterProperty().get().handle(new ActionEvent());
			if (UPDATE.equals(json.getString(MSG_TYPE))) {
				System.out.println("MSG_TYPE : " + json.getString(MSG_TYPE));
				getText().setValue(json.getString(TEXT_CONTENT));
				System.out.println("TEXT_CONTENT : " + json.getString(TEXT_CONTENT));
			}
			super.handleMessage(json);
		}

		public ObjectProperty<EventHandler<ActionEvent>> getEnterProperty() {
			return enterProperty;
		}

	}

	public class CheckBoxHtmlDomNode extends HtmlDomNode {
		private static final String CHECKED = "checked";

		private Property<Boolean> checked = new SimpleBooleanProperty(false);

		public CheckBoxHtmlDomNode() {
			super("input");
		}

		public Property<Boolean> getChecked() {
			return checked;
		}

		@Override
		public void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			super.fillJson(parentNodeJs, jsonObj);
			jsonObj.put("type", "checkbox");
			jsonObj.put(CHECKED, checked.getValue());
		}

		@Override
		public void handleMessage(JsonObject json) {
			if ("checkbox".equals(json.getString(ELT_TYPE)))
				getChecked().setValue(json.getBoolean(CHECKED));
			super.handleMessage(json);
		}
	}

}
