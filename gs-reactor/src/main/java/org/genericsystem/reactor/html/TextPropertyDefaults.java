package org.genericsystem.reactor.html;

import java.util.function.Function;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;

public interface TextPropertyDefaults<M extends Model> {

	public static final String TEXT = "text";

	default Property<String> getTextProperty(Tag<M> tag, M model) {
		if (!model.containsProperty(tag, TEXT)) {
			model.storeProperty(tag, TEXT, new SimpleStringProperty());
			Property<String> text = tag.getProperty(TEXT, model);
			text.addListener(new WeakChangeListener<>(model.getViewContext(tag).getNode().getTextListener()));
		}
		return tag.getProperty(TEXT, model);
	}

	default void setText(Tag<M> tag, String value) {
		tag.addPrefixBinding(model -> getTextProperty(tag, model).setValue(value));
	}

	default void bindText(Tag<M> tag, Function<M, ObservableValue<String>> applyOnModel) {
		tag.addPrefixBinding(model -> getTextProperty(tag, model).bind(applyOnModel.apply(model)));
	}
}
