package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;

import javafx.collections.ObservableList;

public abstract class AbstractTag implements Tag {

	protected TagNode tagNode;

	public TagNode getTagNode() {
		return tagNode;
	}

	public void setParent(Tag parent) {
		tagNode = parent.getRootTag().buildTagNode(parent);
	}

	@Override
	public ObservableList<Tag> getObservableChildren() {
		return getTagNode().getObservableChildren();
	}

	@Override
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return getTagNode().getParent();
	}
}