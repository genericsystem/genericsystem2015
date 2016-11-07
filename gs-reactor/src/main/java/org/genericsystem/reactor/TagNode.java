package org.genericsystem.reactor;

import javafx.collections.ObservableList;

public interface TagNode {

	public <COMPONENT extends Tag> COMPONENT getParent();

	public ObservableList<Tag> getObservableChildren();

}
