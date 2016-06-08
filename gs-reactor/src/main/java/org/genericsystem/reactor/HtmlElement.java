package org.genericsystem.reactor;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 * @param <NODE>
 */
public abstract class HtmlElement<M extends Model, NODE extends HtmlDomNode> extends Element<M, NODE> {
	private static final String MSG_TYPE = "msgType";
	private static final String ADD = "A";
	private static final String UPDATE = "U";
	private static final String REMOVE = "R";
	private static final String UPDATE_TEXT = "UT";
	private static final String ADD_STYLECLASS = "AC";
	private static final String REMOVE_STYLECLASS = "RC";
	private static final String ADD_STYLE = "AS";
	private static final String REMOVE_STYLE = "RS";

	private static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	private static final String NEXT_ID = "nextId";
	private static final String STYLE_PROPERTY = "styleProperty";
	private static final String STYLE_VALUE = "styleValue";
	private static final String STYLECLASS = "styleClass";
	private static final String TEXT_CONTENT = "textContent";
	private static final String TAG_HTML = "tagHtml";
	private static final String ELT_TYPE = "eltType";

	private final String tag;

	protected <PARENTMODEL extends Model, PARENTNODE extends HtmlDomNode> HtmlElement(Element<PARENTMODEL, PARENTNODE> parent, String tag,
			Class<NODE> nodeClass) {
		super(parent, nodeClass);
		this.tag = tag;
	}

	public ServerWebSocket getWebSocket() {
		return ((HtmlElement<?, ?>) getParent()).getWebSocket();
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected Function<NODE, List> getGraphicChildren() {
		return HtmlDomNode::getChildren;
	}

	public void forEach(ObservableListExtractor observableListExtractor) {
		forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, observableListExtractor, CompositeModel::new);
	}

	public void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor) {
		forEach(stringExtractor, observableListExtractor, CompositeModel::new);
	}

	public void select(StringExtractor stringExtractor, Function<Generic[], Generic> genericSupplier) {
		select(stringExtractor, genericSupplier, CompositeModel::new);
	}

	public void select(Function<Generic[], Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier, constructor);
	}

	public void select_(Function<Generic[], Generic> genericSupplier) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericSupplier, CompositeModel::new);
	}

	public void select(StringExtractor stringExtractor, Class<?> genericClass) {
		select(stringExtractor, genericClass, CompositeModel::new);
	}

	public void select(Class<?> genericClass) {
		select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, genericClass);
	}

	public void addStyleClass(String styleClass) {
		addSetBoot(HtmlDomNode::getStyleClasses, styleClass);
	}

	public void addStyle(String propertyName, String value) {
		addBoot(domNode -> domNode.getStyle(propertyName), value);
	}

	public void setText(String text) {
		addBoot(HtmlDomNode::getText, text);
	}

	public void bindTextBidirectional(Function<M, Property<String>> applyOnModel) {
		addBidirectionalBinding(HtmlDomNode::getText, applyOnModel);
	}

	public void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(HtmlDomNode::getText, applyOnModel);
	}

	public void bindStyle(String propertyName, Function<M, ObservableValue<String>> applyOnModel) {
		addBinding(domNode -> domNode.getStyle(propertyName), applyOnModel);
	}

	public void bindStyle(String propertyName, String initialValue) {
		bindStyle(propertyName, model -> ((CompositeModel) model).getObservableStyle(this, propertyName, initialValue));
	}

	public void bindStyle(String propertyName) {
		bindStyle(propertyName, model -> ((CompositeModel) model).getStyleProperty(this, propertyName));
	}

	public void bindOptionalStyleClass(Function<M, ObservableValue<Boolean>> function, String text) {
		addSetBinding(HtmlDomNode::getStyleClasses, function, text);
	}

	@Override
	protected abstract NODE createNode(Object parent);

	public class HtmlDomNode {

		private final String id;
		private final StringProperty text = new SimpleStringProperty();

		private final Set<String> styleClasses = new HashSet<String>() {

			private static final long serialVersionUID = -7679372997269319684L;

			@Override
			public boolean add(String styleClass) {
				boolean result = super.add(styleClass);
				if (result) {
					System.out.println(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass).encodePrettily());
					sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass));
				}
				return result;
			};

			@Override
			public boolean remove(Object styleClass) {
				boolean result = super.remove(styleClass);
				if (result) {
					System.out.println(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass).encodePrettily());
					sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, id).put(STYLECLASS, styleClass));
				}
				return result;
			};
		};

		private final Map<String, String> styles = new HashMap<String, String>() {

			private static final long serialVersionUID = 3900526227565046414L;

			@Override
			public String put(String propertyName, String value) {
				String result = super.put(propertyName, value);
				if (!Objects.equals(value, result)) {
					JsonObject mapJS = new JsonObject();
					mapJS.put(propertyName, value);
					if (value != null && !value.isEmpty())
						sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, id).put(STYLE_PROPERTY, propertyName).put(STYLE_VALUE, value));
					else
						sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, id).put(STYLE_PROPERTY, propertyName));
				}
				return result;
			}
		};

		public HtmlDomNode() {
			this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
			text.addListener((c, o, n) -> {
				System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, id).put(TEXT_CONTENT, n != null ? n : "").encodePrettily());
				sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, id).put(TEXT_CONTENT, n != null ? n : ""));
			});
		}

		protected List<HtmlDomNode> getChildren() {
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
				System.out.println(jsonObj.encodePrettily());
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
			getWebSocket().writeFinalTextFrame(jsonObj.encode());
		}

		void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			jsonObj.put(PARENT_ID, parentNodeJs.getId());
			jsonObj.put(ID, id);
			jsonObj.put(TAG_HTML, tag);
		}

		public Set<String> getStyleClasses() {
			return styleClasses;
		}

		public Property<String> getStyle(String propertyName) {
			Property<String> property = new SimpleStringProperty(styles.get(propertyName));
			property.addListener((c, o, n) -> styles.put(propertyName, n));
			return property;
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

		@Override
		public void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			super.fillJson(parentNodeJs, jsonObj);
			jsonObj.put("type", "text");
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getEnterProperty().get().handle(new ActionEvent());
			if (UPDATE.equals(json.getString(MSG_TYPE)))
				getText().setValue(json.getString(TEXT_CONTENT));
			super.handleMessage(json);
		}

		public ObjectProperty<EventHandler<ActionEvent>> getEnterProperty() {
			return enterProperty;
		}

	}

	public class CheckBoxHtmlDomNode extends HtmlDomNode {
		private static final String CHECKED = "checked";

		private Property<Boolean> checked = new SimpleBooleanProperty(false);

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
