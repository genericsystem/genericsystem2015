package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.function.Function;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSInputText;
import org.genericsystem.reactor.model.GenericModel;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.util.StringConverter;

public class GSInputTextWithConversion extends GSInputText {

	static final Logger log = LoggerFactory.getLogger(GSInputTextWithConversion.class);

	public GSInputTextWithConversion(GSTag parent) {
		super(parent);

		addStyle("width", "100%");
		addStyle("height", "100%");

		createNewProperty(ReactorStatics.CONVERTER);
		initProperty(ReactorStatics.CONVERTER, model -> getConverter(model));
		createNewProperty(ReactorStatics.VALUE);

		storeProperty(ReactorStatics.INVALID, model -> Bindings.createBooleanBinding(() -> {
			boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
			String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
			if (required && (value == null || value.trim().isEmpty()))
				return true;
			try {
				((StringConverter) getProperty(ReactorStatics.CONVERTER, model).getValue()).fromString(value);
				return false;
			} catch (Exception e) {
				return true;
			}
		}, model.getObservableAttributes(this)));
		bindOptionalStyleClass(ReactorStatics.INVALID, ReactorStatics.INVALID);

		bindBiDirectionalAttributeOnEnter(ReactorStatics.VALUE, ReactorStatics.VALUE, model -> (StringConverter) getProperty(ReactorStatics.CONVERTER, model).getValue());
	}

	private void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName) {
		bindBiDirectionalAttributeOnEnter(propertyName, attributeName, ApiStatics.STRING_CONVERTERS.get(String.class));
	}

	private <T extends Serializable> void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName, StringConverter<T> stringConverter) {
		bindBiDirectionalAttributeOnEnter(propertyName, attributeName, model -> stringConverter);
	}

	private <T extends Serializable> void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName, Function<GenericModel, StringConverter<T>> getStringConverter) {
		bindAction(model -> {
			Property observable = getProperty(propertyName, model);
			StringConverter stringConverter = getStringConverter.apply(model);
			String attributeValue = model.getObservableAttributes(this).get(attributeName);
			try {
				observable.setValue(stringConverter.fromString(attributeValue));
			} catch (Exception ignore) {
				log.warn("Conversion exception : " + ignore.getMessage());
			}
		});
		addPrefixBinding(model -> {
			StringConverter stringConverter = getStringConverter.apply(model);
			ChangeListener listener = (o, old, newValue) -> model.getObservableAttributes(this).put(attributeName, stringConverter.toString(newValue));
			Property observable = getProperty(propertyName, model);
			observable.addListener(listener);
		});
	}

	public StringConverter<?> getConverter(GenericModel model) {
		Class<?> clazz = model.getGeneric().getInstanceValueClassConstraint();
		if (clazz == null)
			clazz = String.class;
		return ApiStatics.STRING_CONVERTERS.get(clazz);
	}

	public static class GSInputTextEditorWithConversion extends GSInputTextWithConversion {

		public GSInputTextEditorWithConversion(GSTag parent) {
			super(parent);
			initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
			bindActionToValueChangeListener(ReactorStatics.VALUE, (model, nva) -> {
				if (nva != null)
					model.getGeneric().updateValue(nva);
			});
		}

		@Override
		public StringConverter<?> getConverter(GenericModel model) {
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