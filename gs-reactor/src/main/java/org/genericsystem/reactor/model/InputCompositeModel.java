package org.genericsystem.reactor.model;

import java.io.Serializable;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.util.StringConverter;

public class InputCompositeModel extends GenericModel {
	private Property<String> inputString = new SimpleStringProperty();
	private ObservableValue<Boolean> invalid = Bindings.createBooleanBinding(() -> !validate(inputString.getValue()), inputString);
	private Property<TriFunction<Generic[], Serializable, Generic, Generic>> inputAction = new SimpleObjectProperty<>();
	private StringConverter<? extends Serializable> stringConverter;
	
	public InputCompositeModel(Generic[] generics, StringExtractor extractor) {
		super(generics, extractor);
		Class<?> clazz = this.getGeneric().getInstanceValueClassConstraint();
		if (clazz == null)
			clazz = String.class;
		setStringConverter(ApiStatics.STRING_CONVERTERS.get(clazz));
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

	public Serializable getValue() {
		return getStringConverter().fromString(inputString.getValue());
	}

	public Property<TriFunction<Generic[], Serializable, Generic, Generic>> getInputAction() {
		return inputAction;
	}

	public StringConverter<? extends Serializable> getStringConverter() {
		return stringConverter;
	}

	public void setStringConverter(StringConverter<? extends Serializable> stringConverter) {
		this.stringConverter = stringConverter;
	}

	@FunctionalInterface
	public interface TriFunction<T, U, R, S> {

	    R apply(T t, U u, S s);
	}
}