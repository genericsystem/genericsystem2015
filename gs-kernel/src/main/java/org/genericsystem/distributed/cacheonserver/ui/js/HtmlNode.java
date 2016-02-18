package org.genericsystem.distributed.cacheonserver.ui.js;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

import org.genericsystem.distributed.GSBuffer;

public class HtmlNode {
	private final ObjectProperty<EventHandler<ActionEvent>> actionProperty = new SimpleObjectProperty<>();
	private final String id;
	private StringProperty tag = new SimpleStringProperty();
	private StringProperty text = new SimpleStringProperty();
	private final ServerWebSocket webSocket;
	private ObservableList<HtmlNode> childrenNode = FXCollections.emptyObservableList();
	private StringProperty style = new SimpleStringProperty("label");
	private ObservableList<String> styleClass = FXCollections.observableArrayList();

	public HtmlNode(ServerWebSocket webSocket) {
		this.id = String.format("%010d", Integer.parseInt(this.hashCode() + "")).substring(0, 10);
		this.webSocket = webSocket;
		text.addListener(new ChangeListener<String>() {
			@Override
			public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "U");
				jsonObj.put("nodeId", id);
				jsonObj.put("textContent", newValue);

				GSBuffer bufferAdmin = new GSBuffer();
				bufferAdmin.appendString(jsonObj.encode());
				webSocket.write(bufferAdmin);
			}
		});

		styleClass.addListener(new ListChangeListener<String>() {

			@Override
			public void onChanged(javafx.collections.ListChangeListener.Change<? extends String> c) {
				JsonArray arrayJS = new JsonArray();
				styleClass.forEach(clazz -> arrayJS.add(clazz));

				JsonObject jsonObj = new JsonObject().put("msg_type", "U");
				jsonObj.put("nodeId", id);
				jsonObj.put("styleClass", arrayJS);

				GSBuffer bufferAdmin = new GSBuffer();
				bufferAdmin.appendString(jsonObj.encode());
				webSocket.write(bufferAdmin);
			}
		});
	}

	public ObservableList<String> getStyleClass() {
		return styleClass;
	}

	public StringProperty getStyle() {
		return style;
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

	public ObservableList<HtmlNode> getChildrenNode() {
		return childrenNode;
	}

	public String getId() {
		return id;
	}

	public StringProperty getTag() {
		return tag;
	}

	public void setTag(String tag) {
		this.tag.set(tag);
	}

	public static class HtmlInputNode extends HtmlNode {
		private String type;

		public HtmlInputNode(ServerWebSocket webSocket, String type) {
			super(webSocket);
			this.type = type;
		}

		public String getType() {
			return type;
		}

	}
}
