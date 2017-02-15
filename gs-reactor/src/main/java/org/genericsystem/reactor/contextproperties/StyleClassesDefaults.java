package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.collections.FXCollections;
import javafx.collections.ObservableSet;

public interface StyleClassesDefaults extends ContextProperty {

	public static final String STYLE_CLASSES = "styleClasses";

	default ObservableSet<String> getDomNodeStyleClasses(Context model) {
		if (!model.containsAttribute((Tag) this, STYLE_CLASSES)) {
			ObservableSet<String> styleClasses = FXCollections.observableSet();
			styleClasses.addListener(model.getHtmlDomNode((Tag) this).getStyleClassesListener());
			addContextAttribute(STYLE_CLASSES, model, styleClasses);
		}
		return getContextAttribute(STYLE_CLASSES, model);
	}
}
