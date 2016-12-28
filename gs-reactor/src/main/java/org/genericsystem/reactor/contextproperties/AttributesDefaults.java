package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;

import javafx.collections.ObservableMap;

public interface AttributesDefaults extends MapStringDefaults {

	public static final String ATTRIBUTES = "attributes";

	default ObservableMap<String, String> getDomNodeAttributes(Context model) {
		return getDomNodeMap(model, ATTRIBUTES, HtmlDomNode::getAttributesListener);
	}
}