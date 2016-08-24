package org.genericsystem.reactor.gs;

import java.io.Serializable;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.HtmlInputText;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.util.StringConverter;

public class GSInputTextWithConversion<T extends Serializable> extends HtmlInputText {

	static final Logger log = LoggerFactory.getLogger(GSInputTextWithConversion.class);

	public GSInputTextWithConversion(GSTag parent) {
		super(parent);
		addStyle("width", "100%");
		addStyle("height", "100%");
		createNewProperty(ReactorStatics.VALUE);
		storeProperty(ReactorStatics.INVALID, model -> Bindings.createBooleanBinding(() -> {
			boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
			String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
			if (required && (value == null || value.trim().isEmpty()))
				return true;
			try {
				getConverter(model).fromString(value);
				return false;
			} catch (Exception e) {
				return true;
			}
		}, model.getObservableAttributes(this)));
		bindOptionalStyleClass(ReactorStatics.INVALID, ReactorStatics.INVALID);
		bindBiDirectionalAttributeOnEnter(ReactorStatics.VALUE, ReactorStatics.VALUE);
	}

	private void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName) {
		bindAction(model -> {
			try {
				getProperty(propertyName, model).setValue(getConverter(model).fromString(model.getObservableAttributes(this).get(attributeName)));
			} catch (Exception ignore) {
				log.warn("Conversion exception : " + ignore.getMessage());
			}
		});
		addPrefixBinding(model -> {
			ChangeListener listener = (o, old, newValue) -> model.getObservableAttributes(this).put(attributeName, getConverter(model).toString((T) newValue));
			getProperty(propertyName, model).addListener(listener);
		});
	}

	public StringConverter<T> getConverter(GenericModel model) {
		Class<?> clazz = model.getGeneric().getInstanceValueClassConstraint();
		if (clazz == null)
			clazz = String.class;
		return ApiStatics.STRING_CONVERTERS.get(clazz);
	}

	public static class GSInputTextEditorWithConversion<T extends Serializable> extends GSInputTextWithConversion<T> implements SelectionDefaults {

		public GSInputTextEditorWithConversion(GSTag parent) {
			super(parent);
			initProperty(ReactorStatics.VALUE, model -> model.getGeneric().getValue());
			addPropertyChangeListener(ReactorStatics.VALUE, (model, nva) -> {
				if (nva != null) {
					Generic updatedGeneric = model.getGeneric().updateValue(nva);
					Property<Generic> genericProperty = getUpdatedGenericProperty(model);
					if (genericProperty != null)
						genericProperty.setValue(updatedGeneric);
				}
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