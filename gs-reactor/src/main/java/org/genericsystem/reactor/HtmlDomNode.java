package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import javafx.beans.value.ChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.SetChangeListener;

public class HtmlDomNode {

	static int count = 0;
	protected static final String MSG_TYPE = "msgType";
	protected static final String ADD = "A";
	protected static final String UPDATE = "U";
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
	protected static final String TEXT_CONTENT = "textContent";
	static final String TAG_HTML = "tagHtml";
	protected static final String ELT_TYPE = "eltType";
	protected static final String SELECTED_INDEX = "selectedIndex";

	private final String id;
	private final String parentId;
	protected ViewContext<?> viewContext;

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

	private final ChangeListener<String> textListener = (o, old, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_TEXT).put(ID, getId()).put(TEXT_CONTENT, newValue != null ? newValue : ""));

	private final ChangeListener<Number> indexListener = (o, old, newValue) -> {
		// System.out.println(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0)
		// .encodePrettily());
		sendMessage(new JsonObject().put(MSG_TYPE, UPDATE_SELECTION).put(ID, getId()).put(SELECTED_INDEX, newValue != null ? newValue : 0));
	};

	public ChangeListener<Number> getIndexListener() {
		return indexListener;
	}

	public ChangeListener<String> getTextListener() {
		return textListener;
	}

	public MapChangeListener<String, String> getStylesListener() {
		return stylesListener;
	}

	public MapChangeListener<String, String> getAttributesListener() {
		return attributesListener;
	}

	public SetChangeListener<String> getStyleClassesListener() {
		return styleClassesListener;
	}

	public HtmlDomNode(String parentId) {
		assert parentId != null;
		this.parentId = parentId;
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
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

	public String getId() {
		return id;
	}

	public void handleMessage(JsonObject json) {

	}
}