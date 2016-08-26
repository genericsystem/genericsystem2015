package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.collections.FXCollections;
import javafx.collections.ObservableSet;
import javafx.collections.WeakSetChangeListener;

public interface StyleClassesDefaults<M extends Model> extends ModelProperty<M> {

	public static final String STYLE_CLASSES = "styleClasses";

	default ObservableSet<String> getDomNodeStyleClasses(Model model) {
		if (!model.containsProperty((Tag<?>) this, STYLE_CLASSES)) {
			createNewInitializedProperty(STYLE_CLASSES, (M) model, m -> {
				ObservableSet<String> styleClasses = FXCollections.observableSet();
				styleClasses.addListener(new WeakSetChangeListener<>(model.getHtmlDomNode((Tag<?>) this).getStyleClassesListener()));
				return styleClasses;
			});
		}
		return (ObservableSet<String>) getProperty(STYLE_CLASSES, model).getValue();
	}
}
