package org.genericsystem.reactor.modelproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;

public interface TextPropertyDefaults<M extends Model> extends ModelProperty<M> {

	public static final String TEXT = "text";
	public static final String TEXT_BINDING = "binding";

	default Property<String> getDomNodeTextProperty(M model) {
		storePropertyWithoutCheck(TEXT, model, m -> new SimpleStringProperty());
		Property<String> text = getProperty(TEXT, model);
		text.addListener(new WeakChangeListener<>(model.getViewContext((Tag<?>) this).getNode().getTextListener()));
		return text;
	}

	default void setText(String value) {
		addPrefixBinding(model -> getDomNodeTextProperty(model).setValue(value));
	}

	default void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		storeProperty(TEXT_BINDING, applyOnModel);
		addPrefixBinding(model -> getDomNodeTextProperty(model).bind(getObservableValue(TEXT_BINDING, model)));
	}
}
