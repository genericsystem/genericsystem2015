package org.genericsystem.api.core.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the class of the instances of a generic.
 *
 * @author Nicolas Feybesse
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface InstanceClass {
	/**
	 * The class of the instances of a generic.
	 *
	 * @return the class of the instances of a generic.
	 */
	Class<?> value();
}
