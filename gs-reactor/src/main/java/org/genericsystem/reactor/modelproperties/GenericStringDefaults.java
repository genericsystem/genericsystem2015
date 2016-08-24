package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;

public interface GenericStringDefaults extends TextPropertyDefaults<GenericModel> {

	public static final String GENERIC_STRING = "genericString";
	public static final String EXTRACTOR = "extractor";

	default ObservableValue<String> getGenericStringProperty(GenericModel model) {
		if (!model.containsProperty((Tag<?>) this, GENERIC_STRING))
			storeProperty(GENERIC_STRING, model, m -> new ReadOnlyStringWrapper(getStringExtractor(m).apply(m.getGeneric())));
		return getProperty(GENERIC_STRING, model);
	}

	default void bindText() {
		addPrefixBinding(model -> getDomNodeTextProperty(model).bind(getGenericStringProperty(model)));
	}

	default StringExtractor getStringExtractor(GenericModel model) {
		Property<StringExtractor> stringExtractorProperty = getProperty(EXTRACTOR, model);
		return stringExtractorProperty != null ? stringExtractorProperty.getValue() : StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default void setStringExtractor(StringExtractor extractor) {
		addPrefixBinding(model -> {
			if (!model.containsProperty((Tag<?>) this, EXTRACTOR))
				storeProperty(EXTRACTOR, model, m -> new ReadOnlyObjectWrapper<>(extractor));
			else
				getProperty(EXTRACTOR, model).setValue(extractor);
		});
	}
}
