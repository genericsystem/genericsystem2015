package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;

public interface StylesDefaults extends MapStringDefaults {

	public static final String STYLES = "styles";

	<T> Property<T> getInheritedProperty(String propertyName, Context[] model, Tag[] tag);

	default ObservableMap<String, String> getDomNodeStyles(Context model) {
		return getDomNodeMap(model, STYLES, HtmlDomNode::getStylesListener);
	}

	default void inheritStyle(Context context, String styleName) {
		Context[] modelArray = new Context[] { context };
		Tag[] tagArray = new Tag[] { (Tag) this };
		Property<ObservableMap<String, String>> ancestorStyles = getInheritedProperty(STYLES, modelArray, tagArray);
		while (ancestorStyles != null)
			if (ancestorStyles.getValue().containsKey(styleName)) {
				ObservableMap<String, String> styles = getDomNodeStyles(context);
				styles.put(styleName, ancestorStyles.getValue().get(styleName));
				ancestorStyles.getValue().addListener((MapChangeListener<String, String>) c -> {
					if (c.getKey().equals(styleName)) {
						if (c.wasRemoved())
							styles.remove(styleName);
						if (c.wasAdded())
							styles.put(styleName, c.getValueAdded());
					}
				});
				break;
			} else
				ancestorStyles = getInheritedProperty(STYLES, modelArray, tagArray);
	}

	default void inheritStyle(String styleName) {
		addPrefixBinding(context -> inheritStyle(context, styleName));
	}
}
