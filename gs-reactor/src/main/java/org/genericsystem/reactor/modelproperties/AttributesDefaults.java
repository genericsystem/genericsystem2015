package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;

import javafx.collections.ObservableMap;

public interface AttributesDefaults<M extends Model> extends MapStringDefaults<M> {

	public static final String ATTRIBUTES = "attributes";

	default ObservableMap<String, String> getDomNodeAttributes(Model model) {
		return getDomNodeMap(model, ATTRIBUTES, HtmlDomNode::getAttributesListener);
	}
}