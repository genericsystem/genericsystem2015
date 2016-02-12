package org.genericsystem.distributed.cacheonserver.ui.js;

import io.vertx.core.buffer.Buffer;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.GSBuffer;

public class NodeJs {

	private final String id;
	private final String data;
	private char type = 'D';
	private ObservableList<NodeJs> childrenNode = FXCollections.emptyObservableList();

	public NodeJs() {
		this.id = (type + "" + this.hashCode()).substring(0, 10);
		data = "Test";
	}

	public ObservableList<NodeJs> getChildrenNode() {
		return childrenNode;
	}

	public String getId() {
		return id;
	}

	public String getData() {
		return data;
	}

	public NodeJs(char type) {
		this.type = type;
		this.id = (type + "" + this.hashCode()).substring(0, 10);
		data = "Test";
		System.out.println(id);
	}

	public Buffer getBuffer() {
		return new GSBuffer().appendString(this.id + this.data);
	}
}
