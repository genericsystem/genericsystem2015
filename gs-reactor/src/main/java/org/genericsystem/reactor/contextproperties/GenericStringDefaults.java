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
		if (!context.containsAttribute((Tag) this, GENERIC_STRING))
			addContextAttribute(GENERIC_STRING, context, Bindings.createStringBinding(() -> {
				Property<StringExtractor> stringExtractor = getStringExtractorProperty(context);
				return stringExtractor.getValue() != null ? stringExtractor.getValue().apply(context.getGeneric()) : StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric());
			}, getStringExtractorProperty(context)));
		return getContextObservableValue(GENERIC_STRING, context);
	}

	default void bindText() {
		addPrefixBinding(context -> bindText(context));
	}

	default void bindText(Context context) {
		getDomNodeTextProperty(context).bind(getGenericStringProperty(context));
	}

	default Property<StringExtractor> getStringExtractorProperty(Context context) {
		if (!context.containsAttribute((Tag) this, EXTRACTOR))
			createNewInitializedProperty(EXTRACTOR, context, null);
		return getContextProperty(EXTRACTOR, context);
	}

	default void setStringExtractor(Context context, StringExtractor extractor) {
		getStringExtractorProperty(context).setValue(extractor);
	}

	default void setStringExtractor(StringExtractor extractor) {
		addPrefixBinding(context -> setStringExtractor(context, extractor));
	}
}
