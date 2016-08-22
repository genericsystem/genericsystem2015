package org.genericsystem.reactor.html;

import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;

public interface TextPropertyDefaults<M extends Model> {

	public static final String TEXT = "text";

	<T> void storePropertyWithoutCheck(String propertyName, M model, Function<M, ObservableValue<T>> applyOnModel);

	void addPrefixBinding(Consumer<M> consumer);

	<T> Property<T> getProperty(String property, Model model);

	default Property<String> getTextProperty(M model) {
		storePropertyWithoutCheck(TEXT, model, m -> new SimpleStringProperty());
		Property<String> text = getProperty(TEXT, model);
		text.addListener(new WeakChangeListener<>(model.getViewContext((Tag<?>) this).getNode().getTextListener()));
		return text;
	}

	default void setText(String value) {
		addPrefixBinding(model -> getTextProperty(model).setValue(value));
	}

	default void bindText(Function<M, ObservableValue<String>> applyOnModel) {
		addPrefixBinding(model -> getTextProperty(model).bind(applyOnModel.apply(model)));
	}
}
