package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;

import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

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
		return getTagNode().getObservableChildren();
	}

	@Override
	public ObservableMap<String, String> getObservableStyles() {
		return getTagNode().getObservableStyles();
	}

	@Override
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return (COMPONENT) parent;
	}
}