package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;

import javafx.collections.ObservableList;

public abstract class AbstractTag implements Tag {

	protected TagNode tagNode;
	private Tag parent;

	public TagNode getTagNode() {
		return tagNode;
	}

	public void setTagNode(TagNode tagNode) {
		this.tagNode = tagNode;
	}

	public void setParent(Tag parent) {
		this.parent = parent;
	}

	@Override
	public ObservableList<Tag> getObservableChildren() {
		// System.out.println("getObservableChildren on " + this);
		return getTagNode().getObservableChildren();
	}

	@Override
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return (COMPONENT) parent;
	}
}