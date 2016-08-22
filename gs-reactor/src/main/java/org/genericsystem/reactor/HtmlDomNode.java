package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.util.function.Consumer;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;
import javafx.collections.SetChangeListener;
import javafx.collections.WeakMapChangeListener;
import javafx.collections.WeakSetChangeListener;

public class HtmlDomNode {

	static int count = 0;
	static final String MSG_TYPE = "msgType";
	static final String ADD = "A";
	private static final String UPDATE = "U";
	static final String REMOVE = "R";
	static final String UPDATE_TEXT = "UT";
	private static final String UPDATE_SELECTION = "US";
	static final String ADD_STYLECLASS = "AC";
	static final String REMOVE_STYLECLASS = "RC";
	static final String ADD_STYLE = "AS";
	static final String REMOVE_STYLE = "RS";
	static final String ADD_ATTRIBUTE = "AA";
	static final String REMOVE_ATTRIBUTE = "RA";

	static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	static final String NEXT_ID = "nextId";
	static final String STYLE_PROPERTY = "styleProperty";
	static final String STYLE_VALUE = "styleValue";
	static final String ATTRIBUTE_NAME = "attributeName";
	static final String ATTRIBUTE_VALUE = "attributeValue";
	static final String STYLECLASS = "styleClass";
	static final String TEXT_CONTENT = "textContent";
	static final String TAG_HTML = "tagHtml";
	private static final String ELT_TYPE = "eltType";

	private final String id;
	private final String parentId;
	ViewContext viewContext;
	private final ObservableSet<String> styleClasses = FXCollections.observableSet();
	private final ObservableMap<String, String> styles = FXCollections.observableHashMap();
	private final ObservableMap<String, String> attributes = FXCollections.observableHashMap();

	private final MapChangeListener<String, String> stylesListener = change -> {
		if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()));
		else if (change.wasAdded())
			sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLE).put(ID, getId()).put(STYLE_PROPERTY, change.getKey()).put(STYLE_VALUE, change.getValueAdded()));
	};

	private final MapChangeListener<String, String> attributesListener = change -> {
		if (!change.wasAdded() || change.getValueAdded() == null || change.getValueAdded().equals(""))
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()));
		else if (change.wasAdded())
			sendMessage(new JsonObject().put(MSG_TYPE, ADD_ATTRIBUTE).put(ID, getId()).put(ATTRIBUTE_NAME, change.getKey()).put(ATTRIBUTE_VALUE, change.getValueAdded()));
	};

	private final SetChangeListener<String> styleClassesListener = change -> {
		if (change.wasAdded())
			sendMessage(new JsonObject().put(MSG_TYPE, ADD_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementAdded()));
		else
			sendMessage(new JsonObject().put(MSG_TYPE, REMOVE_STYLECLASS).put(ID, getId()).put(STYLECLASS, change.getElementRemoved()));
	};

	public ChangeListener<String> getTextListener() {
		return (o, old, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).put(TEXT_CONTENT, newValue != null ? newValue : ""));
	}

	public ObservableMap<String, String> getStyles() {
		return styles;
	}

	public ObservableMap<String, String> getAttributes() {
		return attributes;
	}

	public HtmlDomNode(String parentId) {
		assert parentId != null;
		this.parentId = parentId;
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		styles.addListener(new WeakMapChangeListener<>(stylesListener));
		styleClasses.addListener(new WeakSetChangeListener<>(styleClassesListener));
		attributes.addListener(new WeakMapChangeListener<>(attributesListener));
	}

	public void sendAdd(int index) {
		JsonObject jsonObj = new JsonObject().put(MSG_TYPE, ADD);
		jsonObj.put(PARENT_ID, parentId);
		jsonObj.put(ID, id);
		jsonObj.put(TAG_HTML, viewContext.getTag().getTag());
		jsonObj.put(NEXT_ID, index);
		fillJson(jsonObj);
		// System.out.println(jsonObj.encodePrettily());
		sendMessage(jsonObj);
	}

	public JsonObject fillJson(JsonObject jsonObj) {
		return null;
	}

	public void sendRemove() {
		sendMessage(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id));
		// System.out.println(new JsonObject().put(MSG_TYPE, REMOVE).put(ID, id).encodePrettily());
	}

	public void sendMessage(JsonObject jsonObj) {
		jsonObj.put("count", count++);
		// if (jsonObj.getString(MSG_TYPE).equals(ADD) || jsonObj.getString(MSG_TYPE).equals(REMOVE))
		// System.out.println(jsonObj.encodePrettily());
		getWebSocket().writeFinalTextFrame(jsonObj.encode());
	}

	public ServerWebSocket getWebSocket() {
		return viewContext.getWebSocket();
	}

	public ObservableSet<String> getStyleClasses() {
		return styleClasses;
	}

	public Property<String> getStyle(String propertyName) {
		Property<String> property = new SimpleStringProperty(styles.get(propertyName));
		property.addListener((c, o, n) -> styles.put(propertyName, n));
		return property;
	}

	public String getId() {
		return id;
	}

	public void handleMessage(JsonObject json) {

	}

	public static class ActionHtmlNode extends HtmlDomNode {
		public ActionHtmlNode(String parentId) {
			super(parentId);
		}

		private final Property<Consumer<Object>> actionProperty = new SimpleObjectProperty<>();

		public Property<Consumer<Object>> getActionProperty() {
			return actionProperty;
		}

		@Override
		public void handleMessage(JsonObject json) {
			getActionProperty().getValue().accept(new Object());
		}
	}

	public static class SelectableHtmlDomNode extends ActionHtmlNode {
		private static final String SELECTED_INDEX = "selectedIndex";

		private Property<Number> selectionIndex = new SimpleIntegerProperty();

		private final ChangeListener<Number> indexListener = (o, old, newValue) -> {
			// System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0)
			// .encodePrettily());
			sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0));
		};

		public SelectableHtmlDomNode(String parentId) {
			super(parentId);
			selectionIndex.addListener(new WeakChangeListener<>(indexListener));
		}

		public Property<Number> getSelectionIndex() {
			return selectionIndex;
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (UPDATE.equals(json.getString(MSG_TYPE))) {
				getSelectionIndex().setValue(json.getInteger(SELECTED_INDEX));
				// System.out.println("Selected index : " + getSelectionIndex().getValue());
			}
		}
	}

	public static class InputTextHtmlDomNode extends HtmlDomNode {

		private final Property<String> inputString = new SimpleStringProperty();
		private final Property<Consumer<Object>> enterProperty = new SimpleObjectProperty<>();

		public InputTextHtmlDomNode(String parentId) {
			super(parentId);
			inputString.addListener(new WeakChangeListener<>(inputListener));
		}

		private final ChangeListener<String> inputListener = (o, old, newValue) -> {
			assert old != newValue;
			System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).encodePrettily());
			sendMessage(fillJson(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId())));
		};

		public Property<String> getInputString() {
			return inputString;
		}

		@Override
		public JsonObject fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			return jsonObj.put("type", "text").put(TEXT_CONTENT, inputString.getValue());
		}

		@Override
		public void handleMessage(JsonObject json) {
			if (ADD.equals(json.getString(MSG_TYPE)))
				getEnterProperty().getValue().accept(new Object());
			if (UPDATE.equals(json.getString(MSG_TYPE)))
				getAttributes().put(ReactorStatics.VALUE, json.getString(TEXT_CONTENT));
		}

		public Property<Consumer<Object>> getEnterProperty() {
			return enterProperty;
		}
	}

	public static class InputCheckHtmlDomNode extends HtmlDomNode {
		private final String type;

		public InputCheckHtmlDomNode(String parentId, String type) {
			super(parentId);
			this.type = type;
		}

		@Override
		public JsonObject fillJson(JsonObject jsonObj) {
			super.fillJson(jsonObj);
			return jsonObj.put("type", type);
		}

		@Override
		public void handleMessage(JsonObject json) {
			if ("checkbox".equals(json.getString(ELT_TYPE)))
				getAttributes().put(ReactorStatics.CHECKED, json.getBoolean(ReactorStatics.CHECKED) ? ReactorStatics.CHECKED : "");
		}
	}
}