package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.PasswordDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlInputText;

import java.io.Serializable;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.EncryptionUtils;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.util.StringConverter;

public class GSInputTextWithConversion<T extends Serializable> extends HtmlInputText implements ConvertedValueDefaults {

	static final Logger log = LoggerFactory.getLogger(GSInputTextWithConversion.class);

	public GSInputTextWithConversion() {
		initInput();
	}

	protected void initInput() {
		createConvertedValueProperty();
		storeInvalidProperty(model -> Bindings.createBooleanBinding(() -> {
			boolean required = model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
			String value = getDomNodeAttributes(model).get(VALUE);
			if (value == null || value.trim().isEmpty())
				return required;
			try {
				getConverter(model).fromString(value);
				return false;
			} catch (Exception e) {
				return true;
			}
		}, getDomNodeAttributes(model)));
		bindOptionalStyleClass(INVALID, INVALID);
		bindBiDirectionalAttributeOnEnter(VALUE, VALUE);
	}

	private void bindBiDirectionalAttributeOnEnter(String propertyName, String attributeName) {
		bindAction(model -> {
			try {
				getProperty(propertyName, model).setValue(getConverter(model).fromString(getDomNodeAttributes(model).get(attributeName)));
			} catch (Exception ignore) {
				log.warn("Conversion exception : " + ignore.getMessage());
			}
		});
		addPrefixBinding(model -> {
			ChangeListener listener = (o, old, newValue) -> getDomNodeAttributes(model).put(attributeName, getConverter(model).toString((T) newValue));
			getProperty(propertyName, model).addListener(listener);
		});
	}

	public StringConverter<T> getConverter(Context model) {
		Class<?> clazz = model.getGeneric().getInstanceValueClassConstraint();
		if (clazz == null)
			clazz = String.class;
		return ApiStatics.STRING_CONVERTERS.get(clazz);
	}

	public static class GSInputTextEditorWithConversion<T extends Serializable> extends GSInputTextWithConversion<T> implements SelectionDefaults {

		@Override
		protected void initInput() {
			super.initInput();
			initValueProperty(model -> model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> {
				if (nva != null) {
					Generic updatedGeneric = model.getGeneric().updateValue(nva);
					Property<Generic> genericProperty = getUpdatedGenericProperty(model);
					if (genericProperty != null)
						genericProperty.setValue(updatedGeneric);
				}
			});
		}

		@Override
		public StringConverter<T> getConverter(Context model) {
			Class<?> clazz = model.getGenerics()[1].getInstanceValueClassConstraint();
			if (clazz == null) {
				if (model.getGeneric().getValue() != null)
					clazz = model.getGeneric().getValue().getClass();
				else
					clazz = String.class;
			}
			return ApiStatics.STRING_CONVERTERS.get(clazz);
		}
	}

	public static class PasswordInput extends GSInputTextWithConversion<byte[]> implements PasswordDefaults {
		public PasswordInput() {
			addAttribute("type", "password");
		}

		@Override
		public StringConverter<byte[]> getConverter(Context context) {
			return new StringConverter<byte[]>() {

				@Override
				public String toString(byte[] hash) {
					return null;
				}

				@Override
				public byte[] fromString(String password) {
					if (password.length() < 8)
						throw new IllegalStateException("Password must be at least 8 characters.");
					Property<byte[]> saltProperty = getSaltProperty(context);
					if (saltProperty.getValue() == null)
						saltProperty.setValue(EncryptionUtils.generateSalt());
					return EncryptionUtils.getEncryptedPassword(password, saltProperty.getValue());
				}
			};
		}
	}
}