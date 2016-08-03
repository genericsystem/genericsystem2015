package org.genericsystem.reactor.gs;

import java.io.Serializable;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.TagProperty.ConverterProperty;
import org.genericsystem.reactor.TagProperty.InvalidProperty;
import org.genericsystem.reactor.TagProperty.ValueProperty;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSInputText;
import org.genericsystem.reactor.model.GenericModel;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.util.StringConverter;

public class GSInputTextWithConversion<T extends Serializable> extends GSInputText {

	static final Logger log = LoggerFactory.getLogger(GSInputTextWithConversion.class);
	protected final TagProperty<StringConverter<T>> converterProperty;
	protected final TagProperty<T> valueProperty;
	protected final TagProperty<Boolean> invalidProperty;

	public GSInputTextWithConversion(GSTag parent) {
		super(parent);

		addStyle("width", "100%");
		addStyle("height", "100%");

		converterProperty = createNewProperty(ConverterProperty::new);
		initProperty(converterProperty, model -> getConverter(model));

		valueProperty = createNewProperty(ValueProperty::new);
		bindBiDirectionalAttributeOnEnter(valueProperty, ReactorStatics.VALUE);

		invalidProperty = storeProperty(InvalidProperty::new, model -> Bindings.createBooleanBinding(() -> {
			boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
			String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
			if (required && (value == null || value.trim().isEmpty()))
				return true;
			try {
				converterProperty.getValue(model.getGeneric()).fromString(value);
				return false;
			} catch (Exception e) {
				return true;
			}
		}, model.getObservableAttributes(this)));
		bindOptionalStyleClass(ReactorStatics.INVALID, invalidProperty);
	}

	private void bindBiDirectionalAttributeOnEnter(TagProperty<T> property, String attributeName) {
		bindAction(model -> {
			StringConverter<T> stringConverter = converterProperty.getValue(model.getGeneric());
			String attributeValue = model.getObservableAttributes(this).get(attributeName);
			try {
				property.setValue(model.getGeneric(), stringConverter.fromString(attributeValue));
			} catch (Exception ignore) {
				log.warn("Conversion exception : " + ignore.getMessage());
			}
		});
		addPrefixBinding(model -> {
			StringConverter<T> stringConverter = converterProperty.getValue(model.getGeneric());
			ChangeListener<T> listener = (o, old, newValue) -> model.getObservableAttributes(this).put(attributeName, stringConverter.toString(newValue));
			property.getProperty(model.getGeneric()).addListener(listener);
		});
	}

	public StringConverter<T> getConverter(GenericModel model) {
		Class<?> clazz = model.getGeneric().getInstanceValueClassConstraint();
		if (clazz == null)
			clazz = String.class;
		return ApiStatics.STRING_CONVERTERS.get(clazz);
	}

	public static class GSInputTextEditorWithConversion<T extends Serializable> extends GSInputTextWithConversion<T> {

		public GSInputTextEditorWithConversion(GSTag parent) {
			super(parent);
			initProperty(valueProperty, model -> (T) model.getGeneric().getValue());
			bindActionToValueChangeListener(valueProperty, (model, nva) -> {
				if (nva != null)
					model.getGeneric().updateValue(nva);
			});
		}

		@Override
		public StringConverter<T> getConverter(GenericModel model) {
			Class<?> clazz = model.getGeneric().getMeta().getInstanceValueClassConstraint();
			if (clazz == null) {
				if (model.getGeneric().getValue() != null)
					clazz = model.getGeneric().getValue().getClass();
				else
					clazz = String.class;
			}
			return ApiStatics.STRING_CONVERTERS.get(clazz);
		}
	}
}