package org.genericsystem.distributed.cacheonserver.ui.js;

import io.vertx.core.buffer.Buffer;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.GSBuffer;

public class NodeJs {

	private final String id;
	private final String tag;
	private char type;
	private ObservableList<NodeJs> childrenNode = FXCollections.emptyObservableList();

	public ObservableList<NodeJs> getChildrenNode() {
		return childrenNode;
	}

	public String getId() {
		return id;
	}

	public String getTag() {
		return tag;
	}

	public NodeJs(char type) {
		this.type = type;
		this.id = (type + "" + this.hashCode()).substring(0, 10);
		tag="TEST";
		//System.out.println(id);
	}

	public Buffer getBuffer() {
		return new GSBuffer().appendString(this.id + this.tag);
	}
}
