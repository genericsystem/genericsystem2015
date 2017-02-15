package org.genericsystem.reactor.contextproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

public interface TextPropertyDefaults extends ContextProperty {

	public static final String TEXT = "text";
	public static final String TEXT_BINDING = "binding";

	default Property<String> getDomNodeTextProperty(Context model) {
		if (!model.containsAttribute((Tag) this, TEXT)) {
			Property<String> text = new SimpleStringProperty();
			text.addListener(model.getHtmlDomNode((Tag) this).getTextListener());
			addContextAttribute(TEXT, model, text);
		}
		return getContextProperty(TEXT, model);
	}

	default void setText(Context context, String value) {
		getDomNodeTextProperty(context).setValue(value);
	}

	default void setText(String value) {
		addPrefixBinding(context -> setText(context, value));
	}

	default void bindText(Context context, ObservableValue<String> observableText) {
		addContextAttribute(TEXT_BINDING, context, observableText);
		getDomNodeTextProperty(context).bind(getContextObservableValue(TEXT_BINDING, context));
	}

	default void bindText(Function<Context, ObservableValue<String>> applyOnModel) {
		addContextAttribute(TEXT_BINDING, applyOnModel);
		addPrefixBinding(model -> getDomNodeTextProperty(model).bind(getContextObservableValue(TEXT_BINDING, model)));
	}
}
