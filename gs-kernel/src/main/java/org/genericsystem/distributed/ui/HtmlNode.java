package org.genericsystem.distributed.ui;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
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
	protected final String id;
	protected StringProperty tag = new SimpleStringProperty();
	protected StringProperty text = new SimpleStringProperty();
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
				System.out.println("change text::" + text);
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

	public void fillJsonAdd(HtmlNode parentNodeJs, JsonObject jsonObj) {
		jsonObj.put("parentId", parentNodeJs.getId());
		jsonObj.put("nodeId", id);
		jsonObj.put("tagHtml", tag);
		jsonObj.put("textContent", text);
		JsonArray arrayJS = new JsonArray();
		styleClass.forEach(clazz -> arrayJS.add(clazz));
		jsonObj.put("styleClass", arrayJS);
	}

	public void fillJsonRemove(JsonObject jsonObj) {
		jsonObj.put("nodeId", id);
	}

	public Property<Boolean> getChecked() {
		return new SimpleObjectProperty<>();
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

	public static class HtmlNodeInput extends HtmlNode {
		private String type;

		public HtmlNodeInput(ServerWebSocket webSocket, String type) {
			super(webSocket);
			this.type = type;
		}

		public String getType() {
			return type;
		}

		@Override
		public void fillJsonAdd(HtmlNode parentNodeJs, JsonObject jsonObj) {
			super.fillJsonAdd(parentNodeJs, jsonObj);
			jsonObj.put("type", type);
		}
	}

	public static class HtmlNodeCheckBox extends HtmlNodeInput {
		private Property<Boolean> checked = new SimpleBooleanProperty(false);

		public HtmlNodeCheckBox(ServerWebSocket webSocket) {
			super(webSocket, "checkbox");
			checked.addListener(new ChangeListener<Boolean>() {

				@Override
				public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
					// JsonObject jsonObj = new JsonObject().put("msg_type", "U");
					// jsonObj.put("nodeId", id);
					// jsonObj.put("ckecked", newValue);
					// GSBuffer bufferAdmin = new GSBuffer();
					// bufferAdmin.appendString(jsonObj.encode());
					// webSocket.write(bufferAdmin);
					System.out.println("checked");
				}
			});
		}

		@Override
		public Property<Boolean> getChecked() {
			return checked;
		}

		@Override
		public void fillJsonAdd(HtmlNode parentNodeJs, JsonObject jsonObj) {
			super.fillJsonAdd(parentNodeJs, jsonObj);
			jsonObj.put("checked", checked.getValue());
		}
	}

}
