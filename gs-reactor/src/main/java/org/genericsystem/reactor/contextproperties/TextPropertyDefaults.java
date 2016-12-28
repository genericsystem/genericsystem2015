package org.genericsystem.reactor.contextproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;

public interface TextPropertyDefaults extends ContextProperty {

	public static final String TEXT = "text";
	public static final String TEXT_BINDING = "binding";

	default Property<String> getDomNodeTextProperty(Context model) {
		if (!model.containsProperty((Tag) this, TEXT)) {
			storeProperty(TEXT, model, m -> new SimpleStringProperty());
			Property<String> text = getProperty(TEXT, model);
			text.addListener(new WeakChangeListener<>(model.getHtmlDomNode((Tag) this).getTextListener()));
		}
		return getProperty(TEXT, model);
	}

	default void setText(Context context, String value) {
		getDomNodeTextProperty(context).setValue(value);
	}

	default void setText(String value) {
		addPrefixBinding(context -> setText(context, value));
	}

	default void bindText(Context context, Function<Context, ObservableValue<String>> applyOnModel) {
		storeProperty(TEXT_BINDING, context, applyOnModel);
		getDomNodeTextProperty(context).bind(getObservableValue(TEXT_BINDING, context));
	}

	default void bindText(Function<Context, ObservableValue<String>> applyOnModel) {
		storeProperty(TEXT_BINDING, applyOnModel);
		addPrefixBinding(model -> getDomNodeTextProperty(model).bind(getObservableValue(TEXT_BINDING, model)));
	}
}
