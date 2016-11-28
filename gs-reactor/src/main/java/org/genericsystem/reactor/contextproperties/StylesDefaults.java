package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.collections.ObservableMap;

public interface StylesDefaults extends MapStringDefaults {

	public static final String STYLES = "styles";

	<T> Property<T> getInheritedProperty(String propertyName, Context[] model, Tag[] tag);

	default ObservableMap<String, String> getDomNodeStyles(Context model) {
		return getDomNodeMap(model, STYLES, HtmlDomNode::getStylesListener, tag -> tag.getObservableStyles());
	}

	default void inheritStyle(String styleName) {
		addPrefixBinding(model -> {
			Context[] modelArray = new Context[] { model };
			Tag[] tagArray = new Tag[] { (Tag) this };
			Property<ObservableMap<String, String>> styles = getInheritedProperty(STYLES, modelArray, tagArray);
			while (styles != null)
				if (styles.getValue().containsKey(styleName)) {
					getDomNodeStyles(model).put(styleName, styles.getValue().get(styleName));
					break;
				} else
					styles = getInheritedProperty(STYLES, modelArray, tagArray);
		});
	}
}
