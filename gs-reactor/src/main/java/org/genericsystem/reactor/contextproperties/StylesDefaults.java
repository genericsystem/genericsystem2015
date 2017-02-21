package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;

import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;

public interface StylesDefaults extends MapStringDefaults {

	public static final String STYLES = "styles";

	<T> T getInheritedContextAttribute(String propertyName, Context[] model, Tag[] tag);

	default ObservableMap<String, String> getDomNodeStyles(Context model) {
		return getDomNodeMap(model, STYLES, HtmlDomNode::getStylesListener);
	}

	default void inheritStyle(Context context, String styleName) {
		Context[] modelArray = new Context[] { context };
		Tag[] tagArray = new Tag[] { (Tag) this };
		ObservableMap<String, String> ancestorStyles = getInheritedContextAttribute(STYLES, modelArray, tagArray);
		while (ancestorStyles != null)
			if (ancestorStyles.containsKey(styleName)) {
				ObservableMap<String, String> styles = getDomNodeStyles(context);
				styles.put(styleName, ancestorStyles.get(styleName));
				ancestorStyles.addListener((MapChangeListener<String, String>) c -> {
					if (c.getKey().equals(styleName)) {
						if (c.wasRemoved())
							styles.remove(styleName);
						if (c.wasAdded())
							styles.put(styleName, c.getValueAdded());
					}
				});
				break;
			} else
				ancestorStyles = getInheritedContextAttribute(STYLES, modelArray, tagArray);
	}

	default void inheritStyle(String styleName) {
		addPrefixBinding(context -> inheritStyle(context, styleName));
	}
}
