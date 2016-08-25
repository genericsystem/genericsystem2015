package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.collections.ObservableMap;

public interface StylesDefaults<M extends Model> extends MapStringDefaults<M> {

	public static final String STYLES = "styles";

	<T> Property<T> getInheritedProperty(String propertyName, Model[] model, Tag<?>[] tag);

	default ObservableMap<String, String> getDomNodeStyles(Model model) {
		return getDomNodeMap(model, STYLES, HtmlDomNode::getStylesListener);
	}

	default void inheritStyle(String styleName) {
		addPrefixBinding(model -> {
			Model[] modelArray = new Model[] { model };
			Tag<?>[] tagArray = new Tag<?>[] { (Tag<?>) this };
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
