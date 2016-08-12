package org.genericsystem.reactor.gs;

import java.util.function.Consumer;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;

public interface GenericStringDefaults {

	public static final String GENERIC_STRING = "genericString";
	public static final String EXTRACTOR = "extractor";

	void addPrefixBinding(Consumer<GenericModel> consumer);

	void createNewProperty(String propertyName);

	<T> Property<T> getProperty(String property, Model model);

	default void createGenericStringProperty() {
		createNewProperty(GENERIC_STRING);
	}

	default ObservableValue<String> getGenericStringProperty(GSTag tag, GenericModel model) {
		if (!model.containsProperty(tag, GENERIC_STRING))
			model.storeProperty(tag, GENERIC_STRING, new ReadOnlyStringWrapper(getStringExtractor(tag, model).apply(model.getGeneric())));
		return tag.getProperty(GENERIC_STRING, model);
	}

	default void bindGenericText(GSTag tag) {
		addPrefixBinding(model -> model.getTextProperty(tag).bind(getGenericStringProperty(tag, model)));
	}

	default StringExtractor getStringExtractor(GSTag tag, GenericModel model) {
		Property<StringExtractor> stringExtractorProperty = tag.getProperty(EXTRACTOR, model);
		return stringExtractorProperty != null ? stringExtractorProperty.getValue() : StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	default void setStringExtractor(GSTag tag, StringExtractor extractor) {
		addPrefixBinding(modelContext -> {
			if (!modelContext.containsProperty(tag, EXTRACTOR))
				modelContext.createNewProperty(tag, EXTRACTOR);
			tag.getProperty(EXTRACTOR, modelContext).setValue(extractor);
		});
	}
}
