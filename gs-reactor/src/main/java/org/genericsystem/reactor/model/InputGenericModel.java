package org.genericsystem.reactor.model;

import java.io.Serializable;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.util.StringConverter;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;

public class InputGenericModel extends GenericModel implements InputableModel {
	protected Property<String> inputString;
	protected ObservableValue<Boolean> invalid;
	protected Property<TriFunction<Generic[], Serializable, Generic, Generic>> inputAction = new SimpleObjectProperty<>();
	protected StringConverter<Serializable> stringConverter;

	public InputGenericModel(Generic[] generics, StringExtractor extractor) {
		super(generics, extractor);
		Class<?> clazz = getInstanceValueClassConstraint();
		setStringConverter(ApiStatics.STRING_CONVERTERS.get(clazz));
		inputString = new SimpleStringProperty();
		invalid = Bindings.createBooleanBinding(() -> !validate(inputString.getValue()), inputString);
	}

	public Class<?> getInstanceValueClassConstraint() {
		Class<?> clazz = this.getGeneric().getInstanceValueClassConstraint();
		if (clazz == null)
			clazz = String.class;
		setStringConverter(ApiStatics.STRING_CONVERTERS.get(clazz));
		return clazz;
	}

	private Boolean validate(String input) {
		boolean required = this.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
		if (required && (inputString.getValue() == null || inputString.getValue().isEmpty()))
			return false;
		try {
			getValue();
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	public ObservableValue<Boolean> getInvalid() {
		return invalid;
	}

	public Property<String> getInputString() {
		return inputString;
	}

	@Override
	public Serializable getValue() {
		return getStringConverter().fromString(inputString.getValue());
	}

	@Override
	public Property<TriFunction<Generic[], Serializable, Generic, Generic>> getInputAction() {
		return inputAction;
	}

	public StringConverter<Serializable> getStringConverter() {
		return stringConverter;
	}

	public void setStringConverter(StringConverter<Serializable> stringConverter) {
		this.stringConverter = stringConverter;
	}

	public static class EditInputGenericModel extends InputGenericModel {
		public EditInputGenericModel(Generic[] generics, StringExtractor extractor) {
			super(generics, extractor);
		}

		@Override
		public Class<?> getInstanceValueClassConstraint() {
			Class<?> clazz = this.getGeneric().getMeta().getInstanceValueClassConstraint();
			if (clazz == null) {
				if (getGeneric().getValue() != null)
					clazz = getGeneric().getValue().getClass();
				else
					clazz = String.class;
			}
			return clazz;
		}
	}
}