package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.collections.ObservableMap;

public interface StylesDefaults<M extends Model> extends MapStringDefaults<M> {

	public static final String STYLES = "styles";


	default ObservableMap<String, String> getDomNodeStyles(Model model) {
		return getDomNodeMap(model, STYLES, HtmlDomNode::getStylesListener);
	}
}
