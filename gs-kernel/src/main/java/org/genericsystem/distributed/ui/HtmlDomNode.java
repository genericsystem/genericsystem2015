package org.genericsystem.distributed.ui;

import io.vertx.core.buffer.Buffer;
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
	private static final String MSG_TYPE = "msg_type";
	private static final String ADD = "A";
	private static final String UPDATE = "U";
	private static final String REMOVE = "R";

	private static final String PARENT_ID = "parentId";
	public static final String ID = "nodeId";
	private static final String STYLECLASS = "styleClass";
	private static final String TEXT_CONTENT = "textContent";
	private static final String TAG_HTML = "tagHtml";

	private final ServerWebSocket webSocket;
	private final ObjectProperty<EventHandler<ActionEvent>> actionProperty = new SimpleObjectProperty<>();
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
			JsonObject jsonObj = new JsonObject().put(MSG_TYPE, UPDATE);
			jsonObj.put(ID, id);
			jsonObj.put(STYLECLASS, arrayJS);
			sendMessage(jsonObj);
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
			sendMessage(jsonObj);
			internal.add(childNode);
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

	public Buffer getBuffer() {
		return new GSBuffer().appendString(this.id + this.tag);
	}

	public ObjectProperty<EventHandler<ActionEvent>> getActionProperty() {
		return actionProperty;
	}

	public String getId() {
		return id;
	}

	public String getTag() {
		return tag;
	}

	public void handleMessage(JsonObject json) {
		if (json.getString("msg_type").equals("A"))
			getActionProperty().get().handle(new ActionEvent());

		if (json.getString("msg_type").equals("U")) {
			if (json.getString("eltType").equals("text"))
				getText().setValue(json.getString("textContent"));
		}
	}

	public static class InputHtmlDomNode extends HtmlDomNode {
		private String type;

		public InputHtmlDomNode(ServerWebSocket webSocket) {
			super(webSocket, "input");
			this.type = "text";
		}

		public InputHtmlDomNode(ServerWebSocket webSocket, String type) {
			super(webSocket, "input");
			this.type = type;
		}

		public String getType() {
			return type;
		}

		@Override
		public void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			super.fillJson(parentNodeJs, jsonObj);
			jsonObj.put("type", type);
		}
	}

	public static class CheckBoxHtmlDomNode extends InputHtmlDomNode {
		private static final String MSG_TYPE = "checked";
		private static final String ELT_TYPE = "eltType";
		private Property<Boolean> checked = new SimpleBooleanProperty(false);

		public CheckBoxHtmlDomNode(ServerWebSocket webSocket) {
			super(webSocket, "checkbox");
			checked.addListener(change -> {
				// JsonObject jsonObj = new JsonObject().put("msg_type", "U");
				// jsonObj.put("nodeId", id);
				// jsonObj.put("ckecked", newValue);
				// sendMessage(jsonObj);
				System.out.println("checked");
			});
		}

		public Property<Boolean> getChecked() {
			return checked;
		}

		@Override
		public void fillJson(HtmlDomNode parentNodeJs, JsonObject jsonObj) {
			super.fillJson(parentNodeJs, jsonObj);
			jsonObj.put(MSG_TYPE, checked.getValue());
		}

		@Override
		public void handleMessage(JsonObject json) {
			super.handleMessage(json);
			if ("checkbox".equals(json.getString(ELT_TYPE)))
				getChecked().setValue(json.getBoolean("checked"));
		}
	}

}
