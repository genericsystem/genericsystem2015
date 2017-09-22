package org.genericsystem.reactor.contextproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import io.reactivex.Observable;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;

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

	default void bindText(Context context, Observable<String> observableText) {
		addContextAttribute(TEXT_BINDING, context, observableText);
		context.getHtmlDomNode((Tag) this).getDisposables().add(observableText.subscribe(newText -> getDomNodeTextProperty(context).setValue(newText)));
	}

	default void bindText(Function<Context, Observable<String>> applyOnModel) {
		addPrefixBinding(context -> bindText(context, applyOnModel.apply(context)));
	}
}
