package org.genericsystem.reactor.modelproperties;

import java.util.function.Function;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.collections.FXCollections;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;
import javafx.collections.WeakMapChangeListener;

public interface MapStringDefaults<M extends Model> extends ModelProperty<M> {

	default ObservableMap<String, String> getDomNodeMap(Model model, String propertyName, Function<HtmlDomNode, MapChangeListener<String, String>> getListener) {
		if (!model.containsProperty((Tag<?>) this, propertyName)) {
			createNewInitializedProperty(propertyName, (M) model, m -> {
				ObservableMap<String, String> map = FXCollections.observableHashMap();
				map.addListener(new WeakMapChangeListener<>(getListener.apply(model.getHtmlDomNode((Tag<?>) this))));
				return map;
			});
		}
		return (ObservableMap<String, String>) getProperty(propertyName, model).getValue();
	}
}
