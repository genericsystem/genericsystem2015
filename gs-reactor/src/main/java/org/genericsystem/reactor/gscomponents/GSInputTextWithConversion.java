package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.PasswordDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlInputText;

import java.io.Serializable;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

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

	public GSInputTextWithConversion(Tag parent) {
		super(parent);
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

		public GSInputTextEditorWithConversion() {
		}

		public GSInputTextEditorWithConversion(Tag parent) {
			super(parent);
		}

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
			addAttribute("placeholder", "********");
		}

		@Override
		public StringConverter<byte[]> getConverter(Context context) {
			return new StringConverter<byte[]>() {

				// TODO: Put encryption methods in a separate class.
				private byte[] getEncryptedPassword(String password, byte[] salt) throws NoSuchAlgorithmException, InvalidKeySpecException {
					String algorithm = "PBKDF2WithHmacSHA1";
					int derivedKeyLength = 160;
					int iterations = 20000;
					KeySpec spec = new PBEKeySpec(password.toCharArray(), salt, iterations, derivedKeyLength);
					SecretKeyFactory f = SecretKeyFactory.getInstance(algorithm);
					return f.generateSecret(spec).getEncoded();
				}

				private byte[] generateSalt() throws NoSuchAlgorithmException {
					SecureRandom random = SecureRandom.getInstance("SHA1PRNG");
					byte[] salt = new byte[8];
					random.nextBytes(salt);
					return salt;
				}

				@Override
				public String toString(byte[] hash) {
					return null;
				}

				@Override
				public byte[] fromString(String password) {
					if (password.length() < 8)
						throw new IllegalStateException("Password must be at least 8 characters.");
					Property<byte[]> saltProperty = getSaltProperty(context);
					try {
						if (saltProperty.getValue() == null)
							saltProperty.setValue(generateSalt());
						return getEncryptedPassword(password, saltProperty.getValue());
					} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
						throw new IllegalStateException("Encryption error", e);
					}
				}
			};
		}
	}
}