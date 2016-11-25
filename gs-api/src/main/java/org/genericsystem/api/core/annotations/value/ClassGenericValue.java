package org.genericsystem.api.core.annotations.value;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the value is a class value.
 * 
 * @author Nicolas Feybesse
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface ClassGenericValue {
	/**
	 * The class value.
	 * 
	 * @return the class value.
	 */
	Class<?> value();
}
