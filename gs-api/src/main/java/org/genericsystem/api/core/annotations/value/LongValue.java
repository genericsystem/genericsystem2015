package org.genericsystem.api.core.annotations.value;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the value is a <code>long</code> value.
 * 
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface LongValue {
	/**
	 * The <code>long</code> value.
	 * 
	 * @return the <code>long</code> value.
	 */
	long value();
}
