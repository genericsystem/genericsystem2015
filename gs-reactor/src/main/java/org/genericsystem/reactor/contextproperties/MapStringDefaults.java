package org.genericsystem.reactor.contextproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;

import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;

public interface MapStringDefaults extends ContextProperty {

	default ObservableMap<String, String> getDomNodeMap(Context model, String propertyName, Function<HtmlDomNode, MapChangeListener<String, String>> getListener) {
		if (!model.containsAttribute((Tag) this, propertyName)) {
			ObservableMap<String, String> map = FXCollections.observableHashMap();
			map.addListener(getListener.apply(model.getHtmlDomNode((Tag) this)));
			addContextAttribute(propertyName, model, map);
		}
		return getContextAttribute(propertyName, model);
	}
}
