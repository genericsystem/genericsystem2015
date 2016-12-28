package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface GenericStringDefaults extends TextPropertyDefaults {

	public static final String GENERIC_STRING = "genericString";
	public static final String EXTRACTOR = "extractor";

	default ObservableValue<String> getGenericStringProperty(Context context) {
		if (!context.containsProperty((Tag) this, GENERIC_STRING))
			storeProperty(GENERIC_STRING, context, m -> Bindings.createStringBinding(() -> {
				Property<StringExtractor> stringExtractor = getStringExtractorProperty(m);
				return stringExtractor.getValue() != null ? stringExtractor.getValue().apply(m.getGeneric()) : StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(m.getGeneric());
			}, getStringExtractorProperty(m)));
		return getObservableValue(GENERIC_STRING, context);
	}

	default void bindText() {
		addPrefixBinding(context -> bindText(context));
	}

	default void bindText(Context context) {
		getDomNodeTextProperty(context).bind(getGenericStringProperty(context));
	}

	default Property<StringExtractor> getStringExtractorProperty(Context context) {
		if (!context.containsProperty((Tag) this, EXTRACTOR))
			createNewInitializedProperty(EXTRACTOR, context, c -> null);
		return getProperty(EXTRACTOR, context);
	}

	default void setStringExtractor(Context context, StringExtractor extractor) {
		getStringExtractorProperty(context).setValue(extractor);
	}

	default void setStringExtractor(StringExtractor extractor) {
		addPrefixBinding(context -> setStringExtractor(context, extractor));
	}
}
