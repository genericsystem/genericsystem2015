package org.genericsystem.distributed.cacheonserver.ui.js;

import io.vertx.core.buffer.Buffer;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

import org.genericsystem.distributed.GSBuffer;

public class HtmlNode {
	private final ObjectProperty<EventHandler<ActionEvent>> actionProperty = new SimpleObjectProperty<>();
	private final String id;
	private final StringProperty tag;
	private char type;
	private ObservableList<HtmlNode> childrenNode = FXCollections.emptyObservableList();

	public HtmlNode(char type) {
		this.type = type;
		this.id = (type + "" + this.hashCode()).substring(0, 10);
		tag = new SimpleStringProperty("valeur initiale NodeJs Constructor");
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
}
