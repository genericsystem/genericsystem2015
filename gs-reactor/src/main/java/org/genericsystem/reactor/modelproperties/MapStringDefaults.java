package org.genericsystem.reactor.modelproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.AbstractTag;

import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;
import javafx.collections.WeakMapChangeListener;

public interface MapStringDefaults extends ContextProperty {

	default ObservableMap<String, String> getDomNodeMap(Context model, String propertyName, Function<HtmlDomNode, MapChangeListener<String, String>> getListener, Function<AbstractTag, ObservableMap<String, String>> getInitialMap) {
		if (!model.containsProperty((Tag) this, propertyName)) {
			createNewInitializedProperty(propertyName, model, m -> {
				ObservableMap<String, String> map = getInitialMap.apply((AbstractTag) this);
				map.addListener(new WeakMapChangeListener<>(getListener.apply(model.getHtmlDomNode((Tag) this))));
				return map;
			});
		}
		return (ObservableMap<String, String>) getProperty(propertyName, model).getValue();
	}
}
