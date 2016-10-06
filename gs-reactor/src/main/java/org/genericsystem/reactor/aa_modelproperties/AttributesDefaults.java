package org.genericsystem.reactor.aa_modelproperties;

import javafx.collections.ObservableMap;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;

public interface AttributesDefaults extends MapStringDefaults {

	public static final String ATTRIBUTES = "attributes";

	default ObservableMap<String, String> getDomNodeAttributes(Context model) {
		return getDomNodeMap(model, ATTRIBUTES, HtmlDomNode::getAttributesListener);
	}
}