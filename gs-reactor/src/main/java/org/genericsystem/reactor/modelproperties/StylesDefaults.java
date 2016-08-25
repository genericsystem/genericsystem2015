package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.collections.FXCollections;
import javafx.collections.ObservableMap;
import javafx.collections.WeakMapChangeListener;

public interface StylesDefaults<M extends Model> extends ModelProperty<M> {

	public static final String STYLES = "styles";

	void addStyle(String propertyName, String value);

	default ObservableMap<String, String> getDomNodeStyles(Model model) {
		if (!model.containsProperty((Tag<?>) this, STYLES)) {
			createNewInitializedProperty(STYLES, (M) model, m -> {
				ObservableMap<String, String> styles = FXCollections.observableHashMap();
				styles.addListener(new WeakMapChangeListener<>(model.getViewContext((Tag<?>) this).getNode().getStylesListener()));
				return styles;
			});
		}
		return (ObservableMap<String, String>) getProperty(STYLES, model).getValue();
	}
}
