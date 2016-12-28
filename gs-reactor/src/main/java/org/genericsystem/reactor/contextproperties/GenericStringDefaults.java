package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.StringExtractor;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

public interface GenericStringDefaults extends TextPropertyDefaults {

	public static final String GENERIC_STRING = "genericString";
	public static final String EXTRACTOR = "extractor";

	default ObservableValue<String> getGenericStringProperty(Context model) {
		if (!model.containsProperty((Tag) this, GENERIC_STRING))
			storeProperty(GENERIC_STRING, model, m -> new ReadOnlyStringWrapper(getStringExtractor(m).apply(m.getGeneric())));
		return getProperty(GENERIC_STRING, model);
	}

	default void bindText() {
		addPrefixBinding(context -> bindText(context));
	}

	default void bindText(Context context) {
		getDomNodeTextProperty(context).bind(getGenericStringProperty(context));
	}

	default StringExtractor getStringExtractor(Context model) {
		Property<StringExtractor> stringExtractorProperty = getProperty(EXTRACTOR, model);
		return stringExtractorProperty != null ? stringExtractorProperty.getValue() : StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default void setStringExtractor(Context context, StringExtractor extractor) {
		if (!context.containsProperty((Tag) this, EXTRACTOR))
			storeProperty(EXTRACTOR, context, m -> {
				Property<StringExtractor> extractorProperty = new SimpleObjectProperty<>(extractor);
				return extractorProperty;
			});
		else
			getProperty(EXTRACTOR, context).setValue(extractor);
	}

	default void setStringExtractor(StringExtractor extractor) {
		addPrefixBinding(context -> setStringExtractor(context, extractor));
	}
}
