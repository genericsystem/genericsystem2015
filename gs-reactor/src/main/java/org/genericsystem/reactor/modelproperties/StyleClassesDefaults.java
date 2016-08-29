package org.genericsystem.reactor.modelproperties;

import javafx.collections.FXCollections;
import javafx.collections.ObservableSet;
import javafx.collections.WeakSetChangeListener;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

public interface StyleClassesDefaults extends ContextProperty {

	public static final String STYLE_CLASSES = "styleClasses";

	default ObservableSet<String> getDomNodeStyleClasses(Context model) {
		if (!model.containsProperty((Tag) this, STYLE_CLASSES)) {
			createNewInitializedProperty(STYLE_CLASSES, model, m -> {
				ObservableSet<String> styleClasses = FXCollections.observableSet();
				styleClasses.addListener(new WeakSetChangeListener<>(model.getHtmlDomNode((Tag) this).getStyleClassesListener()));
				return styleClasses;
			});
		}
		return (ObservableSet<String>) getProperty(STYLE_CLASSES, model).getValue();
	}
}
