package org.genericsystem.distributed.ui;

import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import org.genericsystem.distributed.GSBuffer;

public class HtmlDomNode {
	private static final String MSG_TYPE = "msgType";
	private static final String ADD = "A";
	private static final String UPDATE = "U";
	private static final String REMOVE = "R";

	private static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	public static final String NEXT_ID = "nextId";
	private static final String STYLECLASS = "styleClass";
	private static final String TEXT_CONTENT = "textContent";
	private static final String TAG_HTML = "tagHtml";
	private static final String ELT_TYPE = "eltType";

	private final ServerWebSocket webSocket;

	private final String id;
	private final String tag;
	private final StringProperty text = new SimpleStringProperty();
	private final ObservableList<String> styleClasses = FXCollections.observableArrayList();

	public HtmlDomNode(ServerWebSocket webSocket, String tag) {
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		this.webSocket = webSocket;
		this.tag = tag;
		this.text.addListener((o, oldValue, newValue) -> sendMessage(new JsonObject().put(MSG_TYPE, UPDATE).put(ID, id).put(TEXT_CONTENT, newValue)));
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
		JsonArray arrayJS = new JsonArray();
		styleClasses.forEach(arrayJS::add);
		jsonObj.put(STYLECLASS, arrayJS);
	}

	public ObservableList<String> getStyleClasses() {
		return styleClasses;
	}

	public ServerWebSocket getWebSocket() {
		return webSocket;
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

	public static class ActionHtmlNode extends HtmlDomNode {
		private final ObjectProperty<EventHandler<ActionEvent>> actionProperty = new SimpleObjectProperty<>();

		public ActionHtmlNode(ServerWebSocket webSocket, String tag) {
			super(webSocket, tag);
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

	public static class InputTextHtmlDomNode extends HtmlDomNode {
		private final ObjectProperty<EventHandler<ActionEvent>> enterProperty = new SimpleObjectProperty<>();

		public InputTextHtmlDomNode(ServerWebSocket webSocket) {
			super(webSocket, "input");
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
			if (UPDATE.equals(json.getString(MSG_TYPE)))
				getText().setValue(json.getString(TEXT_CONTENT));
			super.handleMessage(json);
		}

		public ObjectProperty<EventHandler<ActionEvent>> getEnterProperty() {
			return enterProperty;
		}

	}

	public static class CheckBoxHtmlDomNode extends HtmlDomNode {
		private static final String CHECKED = "checked";

		private Property<Boolean> checked = new SimpleBooleanProperty(false);

		public CheckBoxHtmlDomNode(ServerWebSocket webSocket) {
			super(webSocket, "input");
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
