package org.genericsystem.api.core.annotations.value;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the value is an array byte value.
 * 
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface ByteArrayValue {
	/**
	 * The array byte value.
	 * 
	 * @return the array byte value.
	 */
	byte[] value();
}
