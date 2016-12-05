package org.genericsystem.reactor;

import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

public interface TagNode {

	public ObservableList<Tag> getObservableChildren();

	public ObservableMap<String, String> buildObservableStyles();
}
