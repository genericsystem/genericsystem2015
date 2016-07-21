package org.genericsystem.reactor.html;

import java.io.Serializable;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.util.StringConverter;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlInputText<M extends Model> extends Tag<M> {
	private static final Logger log = LoggerFactory.getLogger(Tag.class);

	public HtmlInputText(Tag<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputTextHtmlDomNode createNode(String parentId) {
		return new InputTextHtmlDomNode(parentId);
	}

	public void bindAction(Consumer<M> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
	}

	public void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName) {
		bindBiDirectionalAttributeOnEnter(propertyName, attributeName, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	public <T extends Serializable> void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalAttributeOnEnter(propertyName, attributeName, model -> stringConverter);
	}

	public <T extends Serializable> void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName, Function<M, StringConverter<T>> getStringConverter) {
		bindAction(model -> {
			Property observable = getProperty(propertyName, model);
			StringConverter stringConverter = getStringConverter.apply(model);
			String attributeValue = model.getObservableAttributes(this).get(attributeName);
			try {
				observable.setValue(stringConverter.fromString(attributeValue));
			} catch (Exception ignore) {
				log.warn("Conversion exception : " + ignore.getMessage());
			}
		});
		addPrefixBinding(model -> {
			StringConverter stringConverter = getStringConverter.apply(model);
			ChangeListener listener = (o, old, newValue) -> model.getObservableAttributes(this).put(attributeName, stringConverter.toString(newValue));
			Property observable = getProperty(propertyName, model);
			observable.addListener(listener);
		});
	}
}
