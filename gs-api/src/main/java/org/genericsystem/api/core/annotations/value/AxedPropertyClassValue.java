package org.genericsystem.api.core.annotations.value;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.api.core.IVertex.SystemProperty;

/**
 * Indicates the <code>Class</code> value for an axed property.
 * 
 * @author Nicolas Feybesse
 * @see org.genericsystem.api.core.AxedPropertyClass
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface AxedPropertyClassValue {
	/**
	 * The <code>Class</code> of the axed property.
	 * 
	 * @return the <code>Class</code> of the axed property.
	 */
	Class<? extends SystemProperty> propertyClass();

	/**
	 * The position of the axed property.
	 * 
	 * @return the position of the axed property.
	 */
	int pos();
}
